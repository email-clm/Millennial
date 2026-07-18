module output_mod
    implicit none
contains

    subroutine writeoutput(string, array, path_output, year)
        implicit none
        integer, parameter :: r8 = selected_real_kind(12) 
        
        character(len=*), intent(in)   :: string
        real(r8),         intent(in)   :: array(:) ! Assumed-shape
        character(len=*), intent(in)   :: path_output
        integer,          intent(in)   :: year
        
        character(len=512) :: outputfile
        character(len=16)  :: year_str
        integer            :: ier, file_unit, path_len

        ! (The rest of your working subroutine code goes here...)
        write(year_str, '(I0)') year
        outputfile = trim(path_output) // "/" // trim(string) // trim(year_str) // ".txt"
        
        open(newunit=file_unit, file=trim(outputfile), status='unknown', action='write', iostat=ier)
        if (ier == 0) then
            write(file_unit, *) array
            close(file_unit)
        end if
    end subroutine writeoutput

end module output_mod



module decomp_module
	implicit none
	contains
!	decomposition subroutine start
subroutine decomp(para_array, forc_st, forc_sw, psi, forc_npp, bulkdensity, silt, &
		clay, LMWC, POM, MB, MINERAL, SOILAGG, f_LM_leaching, f_MI_LM_des,&
		f_LM_MI_sor, f_LM_MB_uptake,f_PO_LM_dep, f_MB_MI_sor,f_PO_SO_agg, f_MI_SO_agg,&
		f_SO_PO_break, f_SO_MI_break, f_MB_atm)
		
	implicit none
	integer,parameter :: r8 = selected_real_kind(12) 	! 8 byte real
	real(r8), intent(in) :: para_array(:)	! array of parameters for the decomposition subroutine.
	real(r8), intent(in) :: forc_st    					! soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)
	real(r8), intent(in) :: forc_sw    					! soil moisture (fraction)
	real(r8), intent(in) :: psi      					! soil water potential at saturation for CN code (MPa)
	real(r8), intent(in) :: forc_npp					! net primary productivity (gC/m2/day)
	real(r8), intent(in) :: bulkdensity					! soil bulk density (g/cm3)
	real(r8), intent(in) :: silt						! soil silt content (fraction)
	real(r8), intent(inout)	:: clay						! soil clay content (fraction)
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



!	main program start
	PROGRAM Millennial
!	History
!	Xiaofeng Xu created this code program to play with Millennial model structure (ICOS workshop Mar 14-16, 2016 in Boulder, CO)
!	The code is created in May - June 2016, solely by Xiaofeng XU (xxu@sdsu.edu)
!	Jan 2021, the code has been cleaned to close the three issues identified when Benjamin Bond-Lamberty's team converted to R script; the century model code has been removed.
!	This is a toy version of the Millennial model (C-only version, N P will be added in future updates)

!   Commented out model output for Century framework, added daily outputs for all processes Nov 7, 2024 - X Xu

!   Rose Abramoff modified the equations here to create Millennial V2, described in Abramoff et al. (2022); Xiaofeng brought in the edits by Rose on Nov 15, 2024

!	Xiaofeng Xu modified the code to add the regional capacility to allow the model to be set up for global simulation, with biome-specific paraemeters. June 10, 2026

	use decomp_module
	use output_mod

	implicit none
	integer,parameter 	:: r8 = selected_real_kind(12) 	!8 byte real

    integer :: year, month, day, doy		! year, month, and day for the model simulations		
    integer :: startyear, endyear			! start year and end year for the mdoel simulations
    integer :: days_in_month(12)			! Array to hold the number of days in each month (January to December)
    integer :: max_days						! Variable to hold the maximum number of days for the current month

	integer :: ngrid						! number of grids over the space (#)
	integer :: nr							! number of model steps (day)
	integer :: i							! integer for loop 
	integer :: j							! integer for loop
	integer :: n							! integer for loop
	integer :: ID							! integer for loop
	integer :: flag_initialfile				! flag initial file or not
	integer :: flag_output					! flag saving model output or not
	integer :: flag_annual					! flag saving model output as annual or daily
!	integer :: nbiome						! number of total biomes: default value is set to 15

	integer, dimension(:), allocatable :: mask				! mask for each grid, to assign biome-specific parameters	

 !	for initial file
	character(len = 256) :: initialfilepom					! name of the initial file
	character(len = 256) :: initialfilelmwc					! name of the initial file
	character(len = 256) :: initialfilemb					! name of the initial file
	character(len = 256) :: initialfilemaom					! name of the initial file
	character(len = 256) :: initialfileagg					! name of the initial file
	character(len = 256) :: soilparafile					! name of the soil parameter file
	character(len = 256) :: maskvegsoilfile					! name of the veg mask and soil bd, clay, silt file

	character(len = 128), allocatable :: force_npp_file(:)		! name of the forcing data - npp
	character(len = 128), allocatable :: force_st_file(:)		! name of the forcing date - temperature
	character(len = 128), allocatable :: force_sw_file(:)		! name of the output file - soil moisture

!	for output file
	character(len = 256), dimension(:,:), allocatable :: outputfile		! name of the output file, 2D array for variable and temporal dimmensin
	character(len = 256) :: path_output									! path for saving model output	
	character(len=36), allocatable :: filename(:)						! name of year, month, and day.txt
	character(len = 256) :: folder_path_st								! folder path for soil temperature forcing data
	character(len = 256) :: folder_path_sw								! folder path for soil moisture forcing data
	character(len = 256) :: folder_path_npp								! folder path for NPP forcing data
!	end of defining output file

!	the input data: driving forces
	real(r8), dimension(:,:), allocatable :: forc_st			! soil temperature
	real(r8), dimension(:,:), allocatable :: forc_sw			! soil moisture
	real(r8), dimension(:,:), allocatable :: forc_npp			! npp as input c	
!	end of driving forces

!	key variables to drive this model: semi-driving forces

!!	key variables to track the system over time
!	pools 
	real(r8), dimension(:,:), allocatable :: LMWC				! Low molecular weight C - root exudates and the by-products of exoenzyme: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: POM				! free fragments of plant detritus: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: MB					! microbial biomass C: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: MINERAL			! mineral-associated C: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: SOILAGG			! aggregates associated C: 2D array for temporal and spatial dimension
!	end of pools

!	flux corresponding to the Figure 1 in Abramoff et al. 2018
	real(r8), dimension(:,:), allocatable :: f_LM_leaching	    ! carbon flow - LMWC leaching: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_LM_MI_sor		! carbon flow - LMWC to MINERAL sorption: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_LM_MB_uptake		! carbon flow - LMWC to MB uptake: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_MI_LM_des		! carbon flow - MINERAL to LMWC desorption: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_MI_SO_agg		! carbon flow - MINERAL to SOILAGG aggregation: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_SO_PO_break		! carbon flow - SOILAGG to POM breakdown: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_SO_MI_break		! carbon flow - SOILAGG to MINERAL breakdown: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_PO_LM_dep		! carbon flow - POM to LMWC depolymerization: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_PO_SO_agg		! carbon flow - POM to SOILAGG aggregation: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_MB_MI_sor		! carbon flow - MB to MINERAL sorption: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: f_MB_atm			! carbon flow - MB to CO2: 2D array for temporal and spatial dimension
!	end of flux

	!!	annual average of key variables for ourput
!	pools 
	real(r8), dimension(:,:), allocatable :: yLMWC				! annual average of Low molecular weight C - root exudates and the by-products of exoenzyme: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yPOM				! annual average of free fragments of plant detritus: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yMB				! annual average of microbial biomass C: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yMINERAL			! annual average of mineral-associated C: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: ySOILAGG			! annual average of aggregates associated C: 2D array for temporal and spatial dimension
!	end of pools

!	flux corresponding to the Figure 1 in Abramoff et al. 2018
	real(r8), dimension(:,:), allocatable :: yf_LM_leaching	    ! annual average of carbon flow - LMWC leaching: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_LM_MI_sor		! annual average of carbon flow - LMWC to MINERAL sorption: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_LM_MB_uptake	! annual average of carbon flow - LMWC to MB uptake: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_MI_LM_des		! annual average of carbon flow - MINERAL to LMWC desorption: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_MI_SO_agg		! annual average of carbon flow - MINERAL to SOILAGG aggregation: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_SO_PO_break		! annual average of carbon flow - SOILAGG to POM breakdown: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_SO_MI_break		! annual average of carbon flow - SOILAGG to MINERAL breakdown: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_PO_LM_dep		! annual average of carbon flow - POM to LMWC depolymerization: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_PO_SO_agg		! annual average of carbon flow - POM to SOILAGG aggregation: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_MB_MI_sor		! annual average of carbon flow - MB to MINERAL sorption: 2D array for temporal and spatial dimension
	real(r8), dimension(:,:), allocatable :: yf_MB_atm			! annual average of carbon flow - MB to CO2: 2D array for temporal and spatial dimension
!	end of flux

!	soil properties over time
	real(r8), dimension(:,:), allocatable :: psi_real
!	end of flux

!	soil properties ! bulk density, Clay, and Silt is fraction in present model while they are noted as percentage in the paper.
	real(r8), dimension(:), allocatable :: bulkdensity	!bulk density in g/cm3
	real(r8), dimension(:), allocatable	:: clay			!clay
	real(r8), dimension(:), allocatable	:: silt			!silt
	real(r8), dimension(:), allocatable	:: maxpsi		!maximum soil water potential (MPa)
	real(r8), dimension(:), allocatable	:: vwc			!soil water content (cm3/cm3)
	real(r8), dimension(:), allocatable	:: vwcsat		!soil water content at saturation (cm3/cm3)
	real(r8), dimension(:), allocatable	:: smp_l		!soil moisture potential for LMWC leaching (MPa)
	real(r8), dimension(:), allocatable	:: psisat		!soil water potential at saturation (MPa)
	real(r8), dimension(:), allocatable	:: organic		!organic matter content (fraction)
	real(r8), dimension(:), allocatable	:: psi			!soil water potential (MPa)
	
	real(r8), dimension(:), allocatable	:: k_leaching 	! leaching coefficient for LMWC
	real(r8), dimension(:), allocatable	:: Vm_l			! maximum decomposition rate for LMWC
	real(r8), dimension(:), allocatable	:: km_l 		! half saturation constant for LMWC decomposition
	real(r8), dimension(:), allocatable	:: M_Lmin 		! microbial maintenance respiration for LMWC decomposition
	real(r8), dimension(:), allocatable	:: klmc_min		! minimum decomposition rate for LMWC to MINERAL
	real(r8), dimension(:), allocatable	:: par_pc		! partitioning coefficient for LMWC to sorb to mineral surface
	real(r8), dimension(:), allocatable	:: klmc			! decomposition rate for LMWC to MINERAL
	real(r8), dimension(:), allocatable	:: kes		    ! efficiency of LMWC to MB uptake
	real(r8), dimension(:), allocatable	:: CUEref 		! reference carbon use efficiency for LMWC decomposition	
	real(r8), dimension(:), allocatable	:: CUET			! temperature sensitivity of CUE for LMWC decomposition
	real(r8), dimension(:), allocatable	:: Taeref		! reference temperature for CUE for LMWC decomposition
	real(r8), dimension(:), allocatable	:: Vpom_lmc 	! maximum decomposition rate for POM to LMWC
	real(r8), dimension(:), allocatable	:: kpom			! half saturation constant for POM to LMWC decomposition
	real(r8), dimension(:), allocatable	:: k_POMes		! decomposition rate for POM to LMWC by extracellular enzyme
	real(r8), dimension(:), allocatable	:: kmic_min	    ! half saturation coefficient for MIC to MINERAL
	real(r8), dimension(:), allocatable	:: kmic			! decomposition rate for microbial biomass decomposition
	real(r8), dimension(:), allocatable	:: Vpom_agg		! maximum decomposition rate for POM to SOILAGG
	real(r8), dimension(:), allocatable	:: kpom_agg		! half saturation constant for POM to SOILAGG decomposition
	real(r8), dimension(:), allocatable	:: Vmin_agg		! maximum decomposition rate for MINERAL to SOILAGG
	real(r8), dimension(:), allocatable	:: kmin_agg		! half saturation constant for MINERAL to SOILAGG decomposition
	real(r8), dimension(:), allocatable	:: AGGmax		! maximum capacity of soil to hold C in aggregates
	real(r8), dimension(:), allocatable	:: kagg			! decomposition rate for SOILAGG to MINERAL
	real(r8), dimension(:), allocatable :: pAG_MI		! newly added parameter to adjust AGG to MAOC, the left over goes to POC
!	end

	real(r8), dimension(:), allocatable	:: initial_pom
	real(r8), dimension(:), allocatable	:: initial_lmwc
	real(r8), dimension(:), allocatable	:: initial_mb
	real(r8), dimension(:), allocatable	:: initial_mineral
	real(r8), dimension(:), allocatable	:: initial_soilagg
!	end of key variables

	integer, parameter 		:: soil_par_num = 26
	integer, parameter 		:: nbiome = 11
!	character(len=256) 		:: soil_par_f = './soilpara_in' ! local file name
	integer 				:: ier              			! error code
	character(len=40), dimension(soil_par_num) 	:: soil_par_name	! parameter name
	real(r8), dimension(nbiome, soil_par_num)	:: soil_par			! parameter value

	integer, dimension(nbiome)				:: biomeID		! biome ID for each biome, to link with the mask file! biome ID should be the same as the value in the mask file
	character(len=256), dimension(nbiome)	:: biome_name	! biome name for each biome, to link with the mask file! biome name.
	integer					:: current_day			! current day of the simulation
	days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  
	write(*,*) "This is the regional version of the Millenium model at a daily time step"

	write(*,*) "Please enter the start year of your simulations:"
	read(*,*) startyear
    write(*,*) "Please enter the end year of your simulations:"
	read(*,*) endyear
    nr = int((endyear - startyear + 1) * 365)
    !allocate(filename(nr))

	write(*,*) "Please enter the number for total grids for the regional simulation (including NA):"
	read(*,*) ngrid

	write(*,*) "Please enter the name of the veg mask and soil bd, clay, silt file:"
	read(*,*) maskvegsoilfile

	write(*,*) "Please enter the name of the soil parameter file:"
	read(*,*) soilparafile

	write(*,*) "Please enter the folder path for soil temperature forcing data:"
	read '(A)', folder_path_st
	write(*,*) "here 2:", folder_path_st

	write(*,*) "Please enter the folder path for soil moisture forcing data:"
	read '(A)', folder_path_sw

	write(*,*) "Please enter the folder path for NPP forcing data:"
	read '(A)', folder_path_npp

	write(*,*) "Do you want to save the model output? 1 for YES, 0 for NO"
	read(*,*) flag_output
	write(*,*) "Annual output or daily? 1 for annual, 0 for daily"
	read(*,*) flag_annual

	if(flag_output == 1) then	
	write(*,*) "Please enter the path for saving model output:"
	read '(A)', path_output
	end if
	
!	allocate space for key input data
	allocate(character(len=36) :: filename(1:nr))
	allocate(character(len=128) :: force_st_file(1:nr))
	allocate(character(len=128) :: force_sw_file(1:nr))
	allocate(character(len=128) :: force_npp_file(1:nr))

	allocate(forc_st(1:nr, 1:ngrid))
	allocate(forc_sw(1:nr, 1:ngrid))
	allocate(forc_npp(1:nr, 1:ngrid))
	allocate(psi_real(1:nr, 1:ngrid))
	
	allocate(LMWC(1:nr, 1:ngrid))
	allocate(POM(1:nr, 1:ngrid))
	allocate(MB(1:nr, 1:ngrid))
	allocate(MINERAL(1:nr, 1:ngrid))
	allocate(SOILAGG(1:nr, 1:ngrid))
	
	allocate(f_LM_leaching(1:nr, 1:ngrid))
	allocate(f_MI_LM_des(1:nr, 1:ngrid))
	allocate(f_LM_MI_sor(1:nr, 1:ngrid))
	allocate(f_LM_MB_uptake(1:nr, 1:ngrid))
	allocate(f_PO_LM_dep(1:nr, 1:ngrid))
	allocate(f_MB_MI_sor(1:nr, 1:ngrid))
	allocate(f_PO_SO_agg(1:nr, 1:ngrid))
	allocate(f_MI_SO_agg(1:nr, 1:ngrid))
	allocate(f_SO_PO_break(1:nr, 1:ngrid))
	allocate(f_SO_MI_break(1:nr, 1:ngrid))
	allocate(f_MB_atm(1:nr, 1:ngrid))

	allocate(yLMWC(1:endyear-startyear+1, 1:ngrid))
	allocate(yPOM(1:endyear-startyear+1, 1:ngrid))
	allocate(yMB(1:endyear-startyear+1, 1:ngrid))
	allocate(yMINERAL(1:endyear-startyear+1, 1:ngrid))
	allocate(ySOILAGG(1:endyear-startyear+1, 1:ngrid))
	
	allocate(yf_LM_leaching(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_MI_LM_des(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_LM_MI_sor(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_LM_MB_uptake(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_PO_LM_dep(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_MB_MI_sor(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_PO_SO_agg(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_MI_SO_agg(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_SO_PO_break(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_SO_MI_break(1:endyear-startyear+1, 1:ngrid))
	allocate(yf_MB_atm(1:endyear-startyear+1, 1:ngrid))

	allocate(mask(1:ngrid))
	allocate(bulkdensity(1:ngrid))
	allocate(clay(1:ngrid))
	allocate(silt(1:ngrid))
	allocate(maxpsi(1:ngrid))
	allocate(vwc(1:ngrid))
	
	allocate(smp_l(1:ngrid))
	allocate(psisat(1:ngrid))
	allocate(organic(1:ngrid))
	allocate(psi(1:ngrid))
	
	allocate(initial_pom(1:ngrid))
	allocate(initial_lmwc(1:ngrid))
	allocate(initial_mb(1:ngrid))
	allocate(initial_mineral(1:ngrid))
	allocate(initial_soilagg(1:ngrid))

	allocate(vwcsat(1:nbiome))
	allocate(k_leaching(1:nbiome))
	allocate(Vm_l(1:nbiome))
	allocate(km_l(1:nbiome))
	allocate(M_Lmin(1:nbiome))
	allocate(klmc_min(1:nbiome))
	allocate(par_pc(1:nbiome))
	allocate(klmc(1:nbiome))
	allocate(kes(1:nbiome))
	allocate(CUEref(1:nbiome))
	allocate(CUET(1:nbiome))
	allocate(Taeref(1:nbiome))
	allocate(Vpom_lmc(1:nbiome))
	allocate(kpom(1:nbiome))
	allocate(k_POMes(1:nbiome))
	allocate(kmic_min(1:nbiome))
	allocate(kmic(1:nbiome))
	allocate(Vpom_agg(1:nbiome))
	allocate(kpom_agg(1:nbiome))
	allocate(Vmin_agg(1:nbiome))
	allocate(kmin_agg(1:nbiome))
	allocate(AGGmax(1:nbiome))
	allocate(kagg(1:nbiome))
	allocate(pAG_MI(1:nbiome))
!	end of the allocation

	write(*,*) 'Attempting to read veg, mask, and soil parameters .....'
	open(unit = 10, file=maskvegsoilfile, action='read', iostat=ier)
	if (ier /= 0) then
        print *, "Error: Could not open mas, veg, and soil property file!"
        stop
    end if
	!print *, "here 3", maskvegsoilfile
	do i = 1, ngrid
			read (10,*,iostat=ier) mask(i), bulkdensity(i), clay(i), silt(i), organic(i)
			!if (mask(i) < 0) then
			!	bulkdensity(i) = 0.0
			!	clay(i) = 0.0
			!	silt(i) = 0.0
			!end if
		!	write(*,*) "reading mask ", i, mask(i), "", "reading bulk density ", i, bulkdensity(i), "", "reading clay ", i, clay(i), "", "reading silt ", i, silt(i)
!			if (ier /= 0) then
!			write(*,*)'soilpara: error in reading in mask, veg, and soil parameter information'
!			end if
	end do
	close(10)

	write(*,*) 'Attempting to read soil parameters .....'
	open(unit = 10, file=soilparafile)
	do i = 1, nbiome
	!	read (10,*,iostat=ier) biomeID(i), biome_name(i)
		read (10,*,iostat=ier) (soil_par(i, j), j = 1, soil_par_num)
		!print *, soil_par(i, :)
		if (ier /= 0) then
			write(*,*)'soilpara: error in reading in soilpara_in'
		end if
		
	!	Assign values read from soil parameter file to variables with names that match the
	!	names in the soil parameter file. The order of the parameters in the soil parameter file should be the same as the order of the variables here.
		j = 1
		maxpsi(i)				= soil_par(i,j); j = j + 1
		vwcsat(i)				= soil_par(i,j); j = j + 1
	!	organic(i)				= soil_par(i,j); j = j + 1
		k_leaching(i)			= soil_par(i,j); j = j + 1
		Vm_l(i)					= soil_par(i,j); j = j + 1
		km_l(i)					= soil_par(i,j); j = j + 1
		M_Lmin(i)				= soil_par(i,j); j = j + 1
		klmc_min(i)				= soil_par(i,j); j = j + 1
		par_pc(i)				= soil_par(i,j); j = j + 1
		klmc(i)					= soil_par(i,j); j = j + 1
		kes(i)					= soil_par(i,j); j = j + 1
		CUEref(i)				= soil_par(i,j); j = j + 1
		CUET(i)					= soil_par(i,j); j = j + 1
		Taeref(i)				= soil_par(i,j); j = j + 1
		Vpom_lmc(i)				= soil_par(i,j); j = j + 1
		kpom(i)					= soil_par(i,j); j = j + 1
		k_POMes(i)				= soil_par(i,j); j = j + 1
		kmic_min(i)				= soil_par(i,j); j = j + 1
		kmic(i)					= soil_par(i,j); j = j + 1
		Vpom_agg(i)				= soil_par(i,j); j = j + 1
		kpom_agg(i)				= soil_par(i,j); j = j + 1
		Vmin_agg(i)				= soil_par(i,j); j = j + 1
		kmin_agg(i)				= soil_par(i,j); j = j + 1
		AGGmax(i)				= soil_par(i,j); j = j + 1
		kagg(i)					= soil_par(i,j); j = j + 1
		pAG_MI(i)				= soil_par(i,j); j = j + 1	
	end do
	close(10)

!	AGGmax = AGGmax * (0.0265 * clay * 100.0 + 0.1351)
!	print *, "vwcsat: ", vwcsat, clay, dummy(5)
	write(*,*) "Model inializing!"
	write(*,*) "Please enter the name of the file for initializing POM in the model:"
	read(*,*) initialfilepom
	open(unit = 11, file=initialfilepom)
!	Assign initial Carbon pool values to variables.
!	do i = 1, ngrid
	read (11,*,iostat=ier) (initial_pom(i), i =1, ngrid)
	!write(*,*) "initial POM: ", initial_pom
!	end do

	if (ier /= 0) then
	write(*,*)'Model inializing failed!'
	else
	write(*,*) "Model inialization of POM finished!"
	end if
	close(11)

	write(*,*) "Please enter the name of the file for initializing LMWC in the model:"
	read(*,*) initialfilelmwc
	open(unit = 12, file=initialfilelmwc)

!	Assign initial Carbon pool values to variables.
	read (11,*,iostat=ier) (initial_lmwc(i), i =1, ngrid)

	if (ier /= 0) then
	write(*,*)'Model inializing failed!'
	else
	write(*,*) "Model inialization of LMWC finished!"
	end if
	close(12)

	write(*,*) "Please enter the name of the file for initializing MB the model:"
	read(*,*) initialfilemb
	open(unit = 13, file=initialfilemb)

!	Assign initial Carbon pool values to variables.
	read (13,*,iostat=ier) (initial_mb(i), i =1, ngrid)

	if (ier /= 0) then
	write(*,*)'Model inializing failed!'
	else
	write(*,*) "Model inialization of MB finished!"
	end if
	close(13)

	write(*,*) "Please enter the name of the file for initializing MAOM the model:"
	read(*,*) initialfilemaom
	open(unit = 14, file=initialfilemaom)

!	Assign initial Carbon pool values to variables.
	read (14,*,iostat=ier) (initial_mineral(i), i =1, ngrid)

	if (ier /= 0) then
	write(*,*)'Model inializing failed!'
	else
	write(*,*) "Model inialization of MAOM finished!"
	end if
	close(14)

	write(*,*) "Please enter the name of the file for initializing AGGC the model:"
	read(*,*) initialfileagg
		
	open(unit = 15, file=initialfileagg)

!	Assign initial Carbon pool values to variables.
	read (15,*,iostat=ier) (initial_soilagg(i), i =1, ngrid)

	if (ier /= 0) then
	write(*,*)'Model inializing failed!'
	else
	write(*,*) "Model inialization of AGGC finished!"
	end if
	close(15)
	
	LMWC(1,:)=initial_lmwc
	POM(1,:)=initial_pom
	MB(1,:)=initial_mb
	MINERAL(1,:)=initial_mineral
	SOILAGG(1,:)=initial_soilagg
	!write(*,*) "after model inialization!", LMWC(1,:), POM(1,:), MB(1,:), MINERAL(1,:), SOILAGG(1,:)

	yLMWC = 0.0
	yPOM = 0.0
	yMB = 0.0
	yMINERAL = 0.0
	ySOILAGG = 0.0

	yf_LM_leaching = 0.0
	yf_MI_LM_des = 0.0
	yf_LM_MI_sor = 0.0
	yf_LM_MB_uptake = 0.0
	yf_PO_LM_dep = 0.0
	yf_MB_MI_sor = 0.0
	yf_PO_SO_agg = 0.0
	yf_MI_SO_agg = 0.0
	yf_SO_PO_break = 0.0
	yf_SO_MI_break = 0.0
	yf_MB_atm = 0.0

    ! 1. EXTERNAL LOOP: Years (Example: 2024 to 2026)
    i = 1
	do year = startyear, endyear
    write(*,*) "Model running for year: ", year
        ! 2. MIDDLE LOOP: Months
        do month = 1, 12
            ! Determine the maximum days for the current month
            max_days = days_in_month(month)
            ! no leap year          
            ! 3. INTERNAL LOOP: Days
            do day = 1, max_days   
                ! Internal write: Convert integers into a standardized filename string
                ! I4.4 forces a 4-digit year, I2.2 forces 2-digit months/days with leading zeros
                write(filename(i), '(I4.4, I2.2, I2.2, A4)') &
                   year, month, day, ".txt"
                
                ! Print the generated filename string to the screen
            !    print *, filename(i)
                i = i + 1                
            end do ! Day loop
        end do ! Month loop
	end do ! Year loop

do year = startyear, endyear
	print *, 'read forcing data of ', year
	do doy = 1, 365
		print *, 'read forcing data for day ', doy
		current_day = (year - startyear) * 365 + doy
	!	write(*,*) "current day of the simulation: ", year, startyear, doy, current_day, folder_path_st
		force_st_file(current_day) = trim(folder_path_st) // trim(filename(current_day))
	!	write(*,*) "reading soil temperature forcing data for day ", doy, ": ", force_st_file(current_day)
		call readdata(force_st_file(current_day), forc_st((current_day),1:ngrid), ngrid)
		force_sw_file(current_day) = trim(folder_path_sw) // trim(filename(current_day))
	!	write(*,*) "reading soil moisture forcing data for day ", doy, ": ", force_sw_file(current_day)
		call readdata(force_sw_file(current_day), forc_sw((current_day),1:ngrid), ngrid)
		force_npp_file(current_day) = trim(folder_path_npp) // trim(filename(current_day))
	!	write(*,*) "reading NPP forcing data for day ", doy, ": ", force_npp_file(current_day)
		call readdata(force_npp_file(current_day), forc_npp((current_day),1:ngrid), ngrid)
	end do
	print *, 'Read data end for ', year


do n = 1, ngrid
	if(mask(n) < 0) then
		print *, "This grid is not valid for the model simulation. Skipping this grid.", n
		continue
	else 

	select case (mask(n))
	case (101)
		ID = 1
	case (102)
		ID = 2
	case (103)
		ID = 3
	case (104)
		ID = 4
	case (105)
		ID = 5
	case (106)
		ID = 6
	case (107)
		ID = 7
	case (108)
		ID = 8
	case (109)
		ID = 9
	case (110)
		ID = 10
	case (111)
		ID = 11
	case default
		continue
		print *, "Error: Mask value does not match any biome ID. Skipping this grid."
	end select

	do doy = 1, 365
		current_day = (year - startyear) * 365 + doy	
	!print *, 'Read :', year, ' and current day: ', current_day, doy, bulkdensity(n), clay(n), silt(n), forc_sw((current_day),n), vwcsat(ID), organic(ID)
	call soilpsi(bulkdensity(n), clay(n), silt(n), forc_sw((current_day),n), vwcsat(ID), organic(n), psisat(n), psi(n), smp_l(n))
	!print *, 'after soilpsi :', year, ' and current day: ', current_day, doy, psi(n)

	call decomp(soil_par(ID, 1:nbiome), forc_st((current_day),n), forc_sw((current_day),n), psi(n), forc_npp((current_day),n), bulkdensity(n), silt(n), clay(n), &
		LMWC(current_day, n), POM(current_day, n), MB(current_day, n), MINERAL(current_day, n), SOILAGG(current_day, n), f_LM_leaching(current_day, n), &
		f_MI_LM_des(current_day, n),f_LM_MI_sor(current_day, n), f_LM_MB_uptake(current_day, n),f_PO_LM_dep(current_day, n), f_MB_MI_sor(current_day, n), &
		f_PO_SO_agg(current_day, n), f_MI_SO_agg(current_day, n), f_SO_PO_break(current_day, n), f_SO_MI_break(current_day, n),f_MB_atm(current_day, n))

	!print *, n, current_day, "LMWC: ", LMWC(current_day, n),  "POMC: ",POM(current_day, n), "MBC: ", MB(current_day, n),  "MINERALC: ",MINERAL(current_day, n),  "AGGC: ",SOILAGG(current_day, n)

	yLMWC(year - startyear + 1, n) 		= yLMWC(year - startyear + 1, n) + LMWC(current_day, n) / 365.0
	yPOM(year - startyear + 1, n) 		= yPOM(year - startyear + 1, n) + POM(current_day, n) / 365.0
	yMB(year - startyear + 1, n) 		= yMB(year - startyear + 1, n) + MB(current_day, n) / 365.0
	yMINERAL(year - startyear + 1, n) 	= yMINERAL(year - startyear + 1, n) + MINERAL(current_day, n) / 365.0
	ySOILAGG(year - startyear + 1, n) 	= ySOILAGG(year - startyear + 1, n) + SOILAGG(current_day, n) / 365.0

	yf_LM_leaching(year - startyear + 1, n) 	= yf_LM_leaching(year - startyear + 1, n) + f_LM_leaching(current_day, n) / 365.0
	yf_MI_LM_des(year - startyear + 1, n) 		= yf_MI_LM_des(year - startyear + 1, n) + f_MI_LM_des(current_day, n) / 365.0
	yf_LM_MI_sor(year - startyear + 1, n) 		= yf_LM_MI_sor(year - startyear + 1, n) + f_LM_MI_sor(current_day, n) / 365.0
	yf_LM_MB_uptake(year - startyear + 1, n) 	= yf_LM_MB_uptake(year - startyear + 1, n) + f_LM_MB_uptake(current_day, n) / 365.0
	yf_PO_LM_dep(year - startyear + 1, n) 		= yf_PO_LM_dep(year - startyear + 1, n) + f_PO_LM_dep(current_day, n) / 365.0
	yf_MB_MI_sor(year - startyear + 1, n) 		= yf_MB_MI_sor(year - startyear + 1, n) + f_MB_MI_sor(current_day, n) / 365.0
	yf_PO_SO_agg(year - startyear + 1, n) 		= yf_PO_SO_agg(year - startyear + 1, n) + f_PO_SO_agg(current_day, n) / 365.0
	yf_MI_SO_agg(year - startyear + 1, n) 		= yf_MI_SO_agg(year - startyear + 1, n) + f_MI_SO_agg(current_day, n) / 365.0
	yf_SO_PO_break(year - startyear + 1, n) 	= yf_SO_PO_break(year - startyear + 1, n) + f_SO_PO_break(current_day, n) / 365.0
	yf_SO_MI_break(year - startyear + 1, n) 	= yf_SO_MI_break(year - startyear + 1, n) + f_SO_MI_break(current_day, n) / 365.0
	yf_MB_atm(year - startyear + 1, n) 			= yf_MB_atm(year - startyear + 1, n) + f_MB_atm(current_day, n) / 365.0

	!	update the pools after each iteration 
	if(current_day < nr) then
	LMWC(current_day+1, n)=LMWC(current_day, n)
	POM(current_day+1, n)=POM(current_day, n)
	MB(current_day+1, n)=MB(current_day, n)
	MINERAL(current_day+1, n)=MINERAL(current_day, n)
	SOILAGG(current_day+1, n)=SOILAGG(current_day, n)
	endif

	end do	!	doy loop
	end if	!	mask check
end do	!	grid loop

	if(flag_output ==1) then
			print *, "Writing output for year: ", year
			call writeoutput("LMWC", yLMWC(year-startyear+1, :), path_output, year)
			call writeoutput("POM", yPOM(year-startyear+1, :), path_output, year)
			call writeoutput("MB", yMB(year-startyear+1, :), path_output, year)
			call writeoutput("MINERAL", yMINERAL(year-startyear+1, :), path_output, year)
			call writeoutput("SOILAGG", ySOILAGG(year-startyear+1, :), path_output, year)
			call writeoutput("f_LM_leaching", yf_LM_leaching(year-startyear+1, :), path_output, year)
			call writeoutput("f_MI_LM_des", yf_MI_LM_des(year-startyear+1, :), path_output, year)
			call writeoutput("f_LM_MI_sor", yf_LM_MI_sor(year-startyear+1, :), path_output, year)
			call writeoutput("f_LM_MB_uptake", yf_LM_MB_uptake(year-startyear+1, :), path_output, year)
			call writeoutput("f_PO_LM_dep", yf_PO_LM_dep(year-startyear+1, :), path_output, year)
			call writeoutput("f_MB_MI_sor", yf_MB_MI_sor(year-startyear+1, :), path_output, year)
			call writeoutput("f_PO_SO_agg", yf_PO_SO_agg(year-startyear+1, :), path_output, year)
			call writeoutput("f_MI_SO_agg", yf_MI_SO_agg(year-startyear+1, :), path_output, year)
			call writeoutput("f_SO_PO_break", yf_SO_PO_break(year-startyear+1, :), path_output, year)
			call writeoutput("f_SO_MI_break", yf_SO_MI_break(year-startyear+1, :), path_output, year)
			call writeoutput("f_MB_atm", yf_MB_atm(year-startyear+1, :), path_output, year)		
	end if

end do ! Year loop

	deallocate(filename)
	deallocate(force_st_file)
	deallocate(force_sw_file)
	deallocate(force_npp_file)

	deallocate(forc_st)
	deallocate(forc_sw)
	deallocate(forc_npp)
	deallocate(psi_real)
	
	deallocate(LMWC)
	deallocate(POM)
	deallocate(MB)
	deallocate(MINERAL)
	deallocate(SOILAGG)
	
	deallocate(f_LM_leaching)
	deallocate(f_MI_LM_des)
	deallocate(f_LM_MI_sor)
	deallocate(f_LM_MB_uptake)
	deallocate(f_PO_LM_dep)
	deallocate(f_MB_MI_sor)
	deallocate(f_PO_SO_agg)
	deallocate(f_MI_SO_agg)
	deallocate(f_SO_PO_break)
	deallocate(f_SO_MI_break)
	deallocate(f_MB_atm)

	deallocate(yLMWC)
	deallocate(yPOM)
	deallocate(yMB)
	deallocate(yMINERAL)
	deallocate(ySOILAGG)
	
	deallocate(yf_LM_leaching)
	deallocate(yf_MI_LM_des)
	deallocate(yf_LM_MI_sor)
	deallocate(yf_LM_MB_uptake)
	deallocate(yf_PO_LM_dep)
	deallocate(yf_MB_MI_sor)
	deallocate(yf_PO_SO_agg)
	deallocate(yf_MI_SO_agg)
	deallocate(yf_SO_PO_break)
	deallocate(yf_SO_MI_break)
	deallocate(yf_MB_atm)
	
	deallocate(mask)
	deallocate(bulkdensity)
	deallocate(clay)
	deallocate(silt)
	deallocate(maxpsi)
	deallocate(vwc)
	deallocate(vwcsat)
	deallocate(smp_l)
	deallocate(psisat)
	deallocate(organic)
	deallocate(psi)
	
	deallocate(initial_pom)
	deallocate(initial_lmwc)
	deallocate(initial_mb)
	deallocate(initial_mineral)
	deallocate(initial_soilagg)

	deallocate(k_leaching)
	deallocate(Vm_l)
	deallocate(km_l)
	deallocate(M_Lmin)
	deallocate(klmc_min)
	deallocate(par_pc)
	deallocate(klmc)
	deallocate(kes)
	deallocate(CUEref)
	deallocate(CUET)
	deallocate(Taeref)
	deallocate(Vpom_lmc)
	deallocate(kpom)
	deallocate(k_POMes)
	deallocate(kmic_min)
	deallocate(kmic)
	deallocate(Vpom_agg)
	deallocate(kpom_agg)
	deallocate(Vmin_agg)
	deallocate(kmin_agg)
	deallocate(AGGmax)
	deallocate(kagg)
	deallocate(pAG_MI)

	stop
END PROGRAM Millennial
!	main program end



!	read data subroutine start
subroutine readdata(filename, array, ngrid)
	implicit none
	! 8-bye real kind parameter		
	integer, parameter 		:: r8 = selected_real_kind(12) ! 8 byte real

	! dummy arguments
	character(len=*), intent(in)	:: filename			! keep lend=* for dummy character argument
	integer, intent(in)				:: ngrid			! number of grid cells
	real(r8), intent(out)			:: array(1:ngrid)	! array to store the data read from the file, the size should match ngrid

	integer :: n, ier, file_unit

	! 1. Open the file safely using NEWUNIT (Fortran 2008+)
    ! trim() is critical to remove trailing blank spaces from the filename string
    open(newunit=file_unit, file=trim(filename), status='old', action='read', iostat=ier)

	if (ier /= 0) then
        write(*,*) "ERROR: File '", trim(filename), "' does not exist or cannot be opened!"
        stop ! Halt execution immediately if file is missing
    end if

	! 2. Read the 1D grid data row-by-row
    do n = 1, ngrid
        read(file_unit, *, iostat=ier) array(n)
        
        if (ier /= 0) then
            write(*,*) "ERROR: Failed reading input data at row: ", n
            write(*,*) "Check if ngrid (", ngrid, ") matches the actual lines in the file."
            close(file_unit)
            stop ! Halt execution instead of filling the array with corrupt/empty values
        end if
    end do
    
    ! 3. Clean up
    close(file_unit)
    !print *, "Reading forcing data finished successfully for: ", trim(filename)

end subroutine readdata
!	read data subroutine end



!	hydrological properties start
subroutine soilpsi(bulkdensity, clay, silt, vwc, vwcsat, organic, psisat, psi, smp_l)
    ! This module calculates soil matric potential based on the Community Land Model (CLM)
    implicit none
    
    ! 8-byte real precision parameter
    integer, parameter :: r8 = selected_real_kind(12)
    
    ! Intent Inputs
    real(r8), intent(in)  :: bulkdensity   ! Bulk density (g/cm3)
    real(r8), intent(in)  :: clay          ! Sand/Clay fraction (0-1)
    real(r8), intent(in)  :: silt          ! Silt fraction (0-1)
    real(r8), intent(in)  :: vwc           ! Volumetric water content (m3/m3)
    real(r8), intent(in)  :: vwcsat        ! Volumetric water content at saturation
    real(r8), intent(in)  :: organic       ! Organic matter content (kg/m3)
    
    ! Intent Outputs
    real(r8), intent(out) :: psisat        ! Soil water potential at saturation
    real(r8), intent(out) :: psi           ! Soil water potential (MPa or bars depending on system)
    real(r8), intent(out) :: smp_l         ! Soil matric potential (mm)
    
    ! Local Variables
    real(r8) :: bsw          ! Clapp and Hornberger "b" parameter
    real(r8) :: bsw2         ! Clapp and Hornberger "b" for CN module alternative formulation
    real(r8) :: smp          ! Temporary soil matrix potential
    real(r8) :: sucsat       ! Saturated soil suction
    real(r8) :: s_node       ! Soil wetness ratio (relative saturation)
    real(r8) :: smpmin       ! Minimum lower bound for soil potential
    real(r8) :: om_frac      ! Organic matter fraction
    real(r8) :: om_b         ! Clapp-Hornberger parameter for organic soil (Letts, 2000)
    real(r8) :: organic_max  ! Max organic matter threshold where soil behaves like pure peat
    real(r8) :: om_sucsat    ! Saturated suction for organic matter (Letts, 2000)
    real(r8) :: vwc_safe     ! Internal safe copy of vwc
    
    ! Parameters initialization
    om_sucsat   = 10.3_r8
    smpmin      = -1.0e5_r8   ! NOTE: Check if CLM units here expect millimeters (-1.0e5 mm = -100m) 
                              ! If originally -1._r8, verify if it was meters or MPa.
    organic_max = 130.0_r8
    om_b        = 2.7_r8
    !print *, "Input vwc: ", vwc, bulkdensity, clay, silt, " vwcsat: ", vwcsat, " organic: ", organic
    ! 1. Input Sanity Check
    vwc_safe = vwc
    if (vwcsat <= 0.0_r8) then
        write(*,*) "CRITICAL ERROR: vwcsat is zero or negative! Check soil property inputs."
        stop
    end if
    
    if (vwc > vwcsat) then
        write(*,*) "WARNING: vwc (", vwc, ") exceeds saturated value vwcsat (", vwcsat, "). Capping to saturation."
        vwc_safe = vwcsat
    end if
    
    ! 2. Calculate Organic Matter Influence (Letts et al., 2000)
    om_frac = min(organic / organic_max, 1.0_r8)
    om_frac = max(om_frac, 0.0_r8) ! Protect against anomalous negative organic values
    
    ! 3. Mineral Soil Properties (Campbell / Clapp & Hornberger baseline)
    sucsat = 10.0_r8 * (10.0_r8**(1.88_r8 - 0.0131_r8 * (1.0_r8 - silt - clay)))
    
    ! Weighted linear combination of mineral and organic parameters
    bsw    = (1.0_r8 - om_frac) * (2.91_r8 + 0.159_r8 * clay) + om_frac * om_b
    sucsat = (1.0_r8 - om_frac) * sucsat + om_sucsat * om_frac
    
    ! 4. Core Matric Potential Calculation (smp_l in mm)
    s_node = min(1.0_r8, max(vwc_safe / vwcsat, 0.01_r8))
    
    ! (-sucsat * (s_node ** (-bsw))) calculates suction head. 
    ! We bound it by smpmin to prevent asymptotic calculations at extreme dryness.
    smp   = max(smpmin, (-sucsat * (s_node ** (-bsw))))
    smp_l = smp
    
    ! 5. CN Module Alternative Formulation (psi and psisat)
    bsw2   = -(3.1_r8 + 0.157_r8 * clay - 0.003_r8 * (1.0_r8 - clay - silt))
    psisat = -(exp((1.54_r8 - 0.0095_r8 * (1.0_r8 - clay - silt) + 0.0063_r8 * (100.0_r8 - silt)) * log(10.0_r8)) * 9.8e-5_r8)
    
    ! Calculate final dynamic psi
    if (s_node > 0.0_r8) then
        psi = psisat * ((vwc_safe / vwcsat)**bsw2)
    else
        psi = smpmin ! Backup assignment for zero-moisture edge cases
    end if

end subroutine soilpsi

!	hydrological properties end

