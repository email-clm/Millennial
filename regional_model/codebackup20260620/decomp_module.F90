! # This is the decomposition module for the regional model. It contains the subroutine that calculates the decomposition of soil carbon based on various parameters and inputs.!!
! # History: June 2024, Xiaofeng Xu, seperated this from the main program of the Millennial model
! # The decomposition subroutine takes in various parameters and inputs, such as soil temperature, soil moisture, soil water potential, net primary productivity, soil properties, and the current state of the soil carbon pools. It then calculates the rates of various processes such as leaching, desorption, sorption, microbial uptake, and aggregation based on these inputs and updates the state of the soil carbon pools accordingly.

module decomp_module
	implicit none
	contains
!	decomposition subroutine start
subroutine decomp(para_array, forc_st, forc_sw, psi, forc_npp, bulkdensity, silt, &
		clay, LMWC, POM, MB, MINERAL, SOILAGG, f_LM_leaching, f_MI_LM_des,&
		f_LM_MI_sor, f_LM_MB_uptake,f_PO_LM_dep, f_MB_MI_sor,f_PO_SO_agg, f_MI_SO_agg,&
		f_SO_PO_break, f_SO_MI_break, f_MB_atm)
		
	implicit none
	integer,parameter 		:: r8 = selected_real_kind(12) 	! 8 byte real
	real(r8), intent(in) 	:: para_array(:)				! array of parameters for the decomposition subroutine.
	real(r8), intent(in) 	:: forc_st    					! soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)
	real(r8), intent(in) 	:: forc_sw    					! soil moisture (fraction)
	real(r8), intent(in) 	:: psi      					! soil water potential at saturation for CN code (MPa)
	real(r8), intent(in) 	:: forc_npp						! net primary productivity (gC/m2/day)
	real(r8), intent(in) 	:: bulkdensity					! soil bulk density (g/cm3)
	real(r8), intent(in) 	:: silt							! soil silt content (fraction)
	real(r8), intent(in)	:: clay							! soil clay content (fraction)
	real(r8), intent(inout) :: LMWC  	
	real(r8), intent(inout) :: POM  		
	real(r8), intent(inout) :: MB 		 
	real(r8), intent(inout) :: MINERAL 		 
	real(r8), intent(inout) :: SOILAGG 		 
	real(r8), intent(inout) :: f_LM_leaching 		 
	real(r8), intent(inout) :: f_MI_LM_des 		 
	real(r8), intent(inout) :: f_LM_MI_sor 		 
	real(r8), intent(inout) :: f_LM_MB_uptake 		 
	real(r8), intent(inout) :: f_PO_LM_dep 		
	real(r8), intent(inout) :: f_MB_MI_sor        	 
	real(r8), intent(inout) :: f_PO_SO_agg        	 
	real(r8), intent(inout) :: f_MI_SO_agg        	 
	real(r8), intent(inout) :: f_SO_PO_break		
	real(r8), intent(inout)	:: f_SO_MI_break
	real(r8), intent(inout)	:: f_MB_atm
	
	real(r8) :: k_leaching 
	real(r8) :: Vm_l	
	real(r8) :: km_l 
	real(r8) :: M_Lmin 		
	real(r8) :: klmc_min			
	real(r8) :: par_pc		
	real(r8) :: klmc		
	real(r8) :: kes		
	real(r8) :: CUEref
	real(r8) :: CUET		
	real(r8) :: Taeref	
	real(r8) :: Vpom_lmc 	
	real(r8) :: kpom		
	real(r8) :: k_POMes	
	real(r8) :: kmic_min
	real(r8) :: kmic			
	real(r8) :: Vpom_agg
	real(r8) :: kpom_agg
	real(r8) :: Vmin_agg
	real(r8) :: kmin_agg
	real(r8) :: AGGmax
	real(r8) :: kagg
	real(r8) :: pAG_MI

	! local pointers to implicit out scalars
	!
	! !OTHER LOCAL VARIABLES:

	real		:: temp, temp2, temp3	! temporary variables
	real		:: psi_tem1, psi_tem2
	real		:: k_sorption          		! temporary variable for k of sorption
	real		:: Qmax						! maximum sorption capacity see v2 paper
	real(r8)	:: t_scalar     			! soil temperature scalar for decomp
	real(r8)	:: t_scalar_mb  			! soil temperature scalar for decomp
	real(r8)	:: minpsi, maxpsi    		! limits for soil water scalar for decomp
	real		:: w_scalar     			! soil water scalar for decomp
	real		:: rate_scalar  			! combined rate scalar for decomp
	real		:: pH
	real 		:: f_SO_break
	real(r8)	:: t_scalar_reverse     	! soil temperature scalar for decomp
	real(r8)	:: w_scalar_reverse     	! soil temperature scalar for decomp
	
	!~ !-----------------------------------------------------------------------

		!maxpsi		= para_array(1)
		!vwcsat		= para_array(2)
		!organic	= para_array(3)
		k_leaching 	= para_array(4)
		Vm_l 		= para_array(5)
		km_l 		= para_array(6)
		M_Lmin 		= para_array(7)
		klmc_min 	= para_array(8)		
		par_pc 		= para_array(9)	
		klmc 		= para_array(10)
		kes 		= para_array(11)
		CUEref 		= para_array(12)
		CUET 		= para_array(13)
		Taeref 		= para_array(14)
		Vpom_lmc 	= para_array(15)
		kpom 		= para_array(16)
		k_POMes		= para_array(17)
		kmic_min 	= para_array(18)
		kmic 		= para_array(19)
		Vpom_agg 	= para_array(20)
		kpom_agg 	= para_array(21)
		Vmin_agg 	= para_array(22)
		kmin_agg 	= para_array(23)
		AGGmax 		= para_array(24)
		kagg 		= para_array(25)
		pAG_MI		= para_array(26)

	AGGmax = AGGmax * (0.0265 * clay * 100.0 + 0.1351)	! adjust the AGGmax based on clay content, as per the model's requirements

	t_scalar = 0._r8
	t_scalar_reverse = 0._r8
	temp = (forc_st - 15._r8) / 10._r8
	t_scalar = t_scalar + 2. **(temp)
	t_scalar_reverse = t_scalar_reverse + 0.5**(temp)
	
	t_scalar_mb = 0._r8
	temp = (forc_st - 15._r8) / 10._r8
	t_scalar_mb = t_scalar_mb + 2.**(temp)	
	
!	print *, "t_scalar", t_scalar, minpsi, maxpsi, psi
	minpsi = -10.0_r8
	w_scalar = 0._r8
	maxpsi = -0.01_r8
	pH = 7.0
!	print *, " here ", minpsi / psi, minpsi / maxpsi
	!~ psi_tem1 = minpsi / psi
	!~ psi_tem2 = minpsi / maxpsi
	!~ if (psi > maxpsi) then
	!~ w_scalar = 1.
	!~ end if
	!~ if (psi < minpsi) then
	!~ w_scalar = 0.
	!~ else
	!~ w_scalar = w_scalar + log(psi_tem1) / log(psi_tem2) * 2.3
!~ !	w_scalar = w_scalar + (log(1.0 * minpsi/psi))!/log(1.0 * minpsi/maxpsi))
	!~ end if
	
!	xiaofeng replaced above codes with following
	if (psi > minpsi) then
	w_scalar = w_scalar + (psi-minpsi)*(psi-maxpsi)/((psi-minpsi)*(psi-maxpsi) - &
		(psi-(maxpsi-(maxpsi-minpsi)/3.))*(psi-(maxpsi-(maxpsi-minpsi)/3.)))
	end if
	w_scalar = w_scalar ** 0.5
!	print *, "w_scalar", w_scalar

	!#century temperature function
	!soilTemp <- seq(-20,40,length.out = 100)
	!teff <- c(15.4, 11.75, 29.7, 0.031)
	!tfunc <- (teff[2] + (teff[3]/pi)* atan(pi*teff[4]*(soilTemp - teff[1]))) / (teff[2] + (teff[3]/pi)* atan(pi*teff[4]*(30 - teff[1])))
	t_scalar = (11.75 + (29.7 / 3.1415926) * ATAN(real(3.1415926*0.031*(forc_st - 15.4)))) / &
	(11.75 + (29.7 / 3.1415926) * ATAN(real(3.1415926 * 0.031 *(30.0 - 15.4))))
	t_scalar_mb = t_scalar
	
	!#century water function
	!relwc <- seq(0,1,length.out = 100)
	!wfunc <- 1/(1 + 30 * exp(-9*relwc))
	w_scalar = 1.0 / (1.0 + 30. * EXP(real(-9.0 * forc_sw)))

	! LMWC -> out of system LWMMWC leaching
	if (LMWC > 0._r8) then
        f_LM_leaching = LMWC * k_leaching * t_scalar !* w_scalar ! Xiaofeng removed water impact, after a review paper at GBC June,2017
	end if
	
	! MINERAL -> LWMC  desorption Xu commented out the below desorption in V1, and turned it out on Nov 15, 2024
	if (MINERAL > M_Lmin) then
        f_MI_LM_des = Vm_l * (MINERAL - M_Lmin) / (km_l + MINERAL - M_Lmin) * t_scalar * w_scalar
	else
	f_MI_LM_des = 0.
	end if

! 	LMWC -> MINERAL: This adsorption/desorption function is from Mayes 2012, SSAJ
	klmc_min = (10.0 ** (-0.186 * pH - 0.216)) / 24.0
!	Qmax = 10.0 ** (0.297 * log(clay * 100.0) + 2.855) * 1.35 !* 1.25  ! 1.35 is bulk density to convert Q from mg/kg to g/m2 later 1.35 was used as 1.00 here is incorrect.
!	Xiaofeng replaced the Qmax equation with the one in V2 (Rose, 2022)
	Qmax = bulkdensity * 1000. * (silt + clay) * 100.0 * par_pc			! multiple 100.0 to convert silt + clary from fraction to percentage
	temp = (klmc_min * Qmax * LMWC ) / (2. + klmc_min * LMWC) - MINERAL

! 	This is equation 9 in the publication.
	f_LM_MI_sor = (temp / Qmax + 0.0015) * LMWC / 50. * t_scalar * w_scalar !* t_scalar * w_scalar !* (LMWC / 200) * (LMWC / 200)

	if (f_LM_MI_sor < (LMWC * 0.9)) then
 	f_LM_MI_sor = f_LM_MI_sor 
	else
	f_LM_MI_sor = LMWC * 0.9
	end if
		
!	print *, klmc_min, Qmax, f_LM_MI_sor, LMWC, MINERAL

	! LMWC -> MB
	if (LMWC > 0._r8) then
	f_LM_MB_uptake = LMWC * klmc * t_scalar * w_scalar * MB / (MB + kes) * LMWC / (20. + LMWC)
	temp2 = f_LM_MB_uptake * (1. - (CUEref + CUET * (forc_st - Taeref)))
	if(temp2 < 0._r8) then
	temp2 = 0_r8
	end if
	f_LM_MB_uptake = f_LM_MB_uptake - temp2
	end if

	! POM -> LMWC
! 	This is equation 2 in the publication.
	if (POM > 0._r8) then
        f_PO_LM_dep = Vpom_lmc * POM / (POM + kpom) * t_scalar * w_scalar !* (1. - MB / (MB + k_POMes)) 
	end if

	if(f_PO_LM_dep > (0.9 * POM)) then
	f_PO_LM_dep = 0.9 * POM
	end if
		
	! MB -> MINERAL	
	if (MB > 0._r8 .and. MINERAL < Qmax) then
	f_MB_MI_sor = MB * kmic * 0.15 * t_scalar_mb * w_scalar  !* (MB / 200) * (MB / 200)
	else
	f_MB_MI_sor = 0.
	end if
	
	if(f_MB_MI_sor > 0.9 * MB) then
	f_MB_MI_sor = 0.9 * MB
	end if
	if(f_MB_MI_sor < 0.) then
	f_MB_MI_sor = 0.
	end if
	
	! MB -> ATM
	if (MB > 0._r8) then
        f_MB_atm = temp2 + MB * kmic * t_scalar_mb * w_scalar 
	end if
	
	! POM -> SOILAGG
! 	This is equation 5 in publication.
	if (POM > 0._r8) then
        f_PO_SO_agg = Vpom_agg * POM / (kpom_agg + POM) * (1. - SOILAGG / AGGmax) * t_scalar * w_scalar
	end if
	
	if(f_PO_SO_agg > 0.9 * POM) then
	f_PO_SO_agg = 0.9 * POM
	end if

	! MINERAL -> SOILAGG
! 	This is equation 15 in the publication.
	if (MINERAL > 0._r8) then
        f_MI_SO_agg = Vmin_agg * MINERAL / (kmin_agg + MINERAL) * (1. - SOILAGG / AGGmax) !* t_scalar * w_scalar
	end if

	if(f_MI_SO_agg>0.9 * MINERAL) then
	f_MI_SO_agg = 0.9 * MINERAL
	end if
	
	! SOILAGG -> MINERAL
! 	This is equation 6 in publication.
	if (SOILAGG > 0._r8) then
        f_SO_break = SOILAGG * kagg * t_scalar * w_scalar
	f_SO_PO_break = f_SO_break * (1.0 - pAG_MI)
	f_SO_MI_break = f_SO_break * pAG_MI
	end if

	if((f_PO_LM_dep + f_PO_SO_agg) > POM) then
	temp3 = POM / (f_PO_LM_dep + f_PO_SO_agg)
	f_PO_LM_dep = f_PO_LM_dep * temp3
	f_PO_SO_agg = f_PO_SO_agg * temp3
	end if
	
	LMWC = LMWC + (f_PO_LM_dep + f_MI_LM_des - f_LM_leaching - f_LM_MI_sor - f_LM_MB_uptake - temp2) + forc_npp / 3.
	
	POM = POM + (f_SO_PO_break - f_PO_LM_dep - f_PO_SO_agg) + forc_npp * 2. / 3.
	
	MB = MB + (f_LM_MB_uptake - f_MB_MI_sor - f_MB_atm)
	
	MINERAL = MINERAL + (f_LM_MI_sor + f_MB_MI_sor + f_SO_MI_break - f_MI_LM_des - f_MI_SO_agg)
	
	SOILAGG = SOILAGG + (f_PO_SO_agg + f_MI_SO_agg - f_SO_PO_break - f_SO_MI_break)
	
end subroutine decomp
	! decomposition subroutine end
end module decomp_module
