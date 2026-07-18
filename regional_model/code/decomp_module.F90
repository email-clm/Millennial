! # This is the decomposition module for the regional model. It contains the subroutine that calculates the decomposition of soil carbon based on various parameters and inputs.!!
! # History: June 2024, Xiaofeng Xu, separated this from the main program of the Millennial model
! # The decomposition subroutine takes in various parameters and inputs, such as soil temperature, soil moisture, soil water potential, net primary productivity, soil properties, and the current state of the soil carbon pools. It then calculates the rates of various processes such as leaching, desorption, sorption, microbial uptake, and aggregation based on these inputs and updates the state of the soil carbon pools accordingly.

module decomp_module
    implicit none

contains

    ! decomposition subroutine start
    subroutine decomp(para_array, forc_st, forc_sw, psi, forc_npp, bulkdensity, silt, &
            clay, LMWC, POM, MB, MINERAL, SOILAGG, f_LM_leaching, f_MI_LM_des, &
            f_LM_MI_sor, f_LM_MB_uptake, f_PO_LM_dep, f_MB_MI_sor, f_PO_SO_agg, f_MI_SO_agg, &
            f_SO_PO_break, f_SO_MI_break, f_MB_atm)
        
        implicit none
        integer, parameter       :: r8 = selected_real_kind(12)   ! 8 byte real
        real(r8), intent(in)     :: para_array(:)                 ! array of parameters for the decomposition subroutine.
        real(r8), intent(in)     :: forc_st                       ! soil temperature (Kelvin)
        real(r8), intent(in)     :: forc_sw                       ! soil moisture (fraction)
        real(r8), intent(in)     :: psi                           ! soil water potential at saturation for CN code (MPa)
        real(r8), intent(in)     :: forc_npp                      ! net primary productivity (gC/m2/day)
        real(r8), intent(in)     :: bulkdensity                   ! soil bulk density (g/cm3)
        real(r8), intent(in)     :: silt                          ! soil silt content (fraction)
        real(r8), intent(in)     :: clay                          ! soil clay content (fraction)
        real(r8), intent(inout)  :: LMWC     
        real(r8), intent(inout)  :: POM      
        real(r8), intent(inout)  :: MB        
        real(r8), intent(inout)  :: MINERAL   
        real(r8), intent(inout)  :: SOILAGG   
        real(r8), intent(inout)  :: f_LM_leaching   
        real(r8), intent(inout)  :: f_MI_LM_des     
        real(r8), intent(inout)  :: f_LM_MI_sor     
        real(r8), intent(inout)  :: f_LM_MB_uptake   
        real(r8), intent(inout)  :: f_PO_LM_dep     
        real(r8), intent(inout)  :: f_MB_MI_sor              
        real(r8), intent(inout)  :: f_PO_SO_agg              
        real(r8), intent(inout)  :: f_MI_SO_agg              
        real(r8), intent(inout)  :: f_SO_PO_break        
        real(r8), intent(inout)  :: f_SO_MI_break
        real(r8), intent(inout)  :: f_MB_atm
        
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

        ! local variables converted to r8 precision to avoid type mixing warnings
        real(r8) :: temp, temp2, temp3    
        real(r8) :: Qmax                        
        real(r8) :: t_scalar                    
        real(r8) :: t_scalar_mb             
        real(r8) :: minpsi, maxpsi            
        real(r8) :: w_scalar                    
        real(r8) :: pH
        real(r8) :: f_SO_break
        real(r8) :: t_scalar_reverse         
        real(r8) :: w_scalar_reverse         
        
        ! Initialize all fluxes and intermediate terms to safe defaults
        f_LM_leaching  = 0.0_r8
        f_MI_LM_des    = 0.0_r8
        f_LM_MI_sor    = 0.0_r8
        f_LM_MB_uptake = 0.0_r8
        f_PO_LM_dep    = 0.0_r8
        f_MB_MI_sor    = 0.0_r8
        f_PO_SO_agg    = 0.0_r8
        f_MI_SO_agg    = 0.0_r8
        f_SO_PO_break  = 0.0_r8
        f_SO_MI_break  = 0.0_r8
        f_MB_atm       = 0.0_r8
        temp2          = 0.0_r8
        f_SO_break     = 0.0_r8

        ! Map parameters array
        k_leaching = para_array(4)
        Vm_l       = para_array(5)
        km_l       = para_array(6)
        M_Lmin     = para_array(7)
        klmc_min   = para_array(8)        
        par_pc     = para_array(9)    
        klmc       = para_array(10)
        kes        = para_array(11)
        CUEref     = para_array(12)
        CUET       = para_array(13)
        Taeref     = para_array(14)
        Vpom_lmc   = para_array(15)
        kpom       = para_array(16)
        k_POMes    = para_array(17)
        kmic_min   = para_array(18)
        kmic       = para_array(19)
        Vpom_agg   = para_array(20)
        kpom_agg   = para_array(21)
        Vmin_agg   = para_array(22)
        kmin_agg   = para_array(23)
        AGGmax     = para_array(24)
        kagg       = para_array(25)
        pAG_MI     = para_array(26)

        ! Adjust the AGGmax based on clay content
        AGGmax = AGGmax * (0.0265_r8 * clay * 100.0_r8 + 0.1351_r8)    

        t_scalar = 0._r8
        t_scalar_reverse = 0._r8
        temp = (forc_st - 15._r8) / 10._r8
        t_scalar = t_scalar + 2.0_r8**(temp)
        t_scalar_reverse = t_scalar_reverse + 0.5_r8**(temp)
        
        t_scalar_mb = 0._r8
        temp = (forc_st - 15._r8) / 10._r8
        t_scalar_mb = t_scalar_mb + 2.0_r8**(temp)    
        
        minpsi = -10.0_r8
        w_scalar = 0._r8
        maxpsi = -0.01_r8
        pH = 7.0_r8

        if (psi > minpsi) then
            w_scalar = w_scalar + (psi-minpsi)*(psi-maxpsi)/((psi-minpsi)*(psi-maxpsi) - &
                       (psi-(maxpsi-(maxpsi-minpsi)/3.0_r8))*(psi-(maxpsi-(maxpsi-minpsi)/3.0_r8)))
        end if
        w_scalar = w_scalar ** 0.5_r8

        ! Century temperature function (Fixed type precision assignment inside ATAN)
        t_scalar = (11.75_r8 + (29.7_r8 / 3.14159_r8) * ATAN(real(3.14159_r8*0.031_r8*(forc_st - 15.4_r8), r8))) / &
                   (11.75_r8 + (29.7_r8 / 3.14159_r8) * ATAN(real(3.14159_r8 * 0.031_r8 *(30.0_r8 - 15.4_r8), r8)))
        t_scalar_mb = t_scalar
        
        ! Century water function
        w_scalar = 1.0_r8 / (1.0_r8 + 30.0_r8 * EXP(real(-9.0_r8 * forc_sw, r8)))

        ! LMWC -> out of system LWMMWC leaching
        if (LMWC > 0._r8) then
            f_LM_leaching = LMWC * k_leaching * t_scalar 
        end if
        
        ! MINERAL -> LWMC desorption
        if (MINERAL > M_Lmin) then
            f_MI_LM_des = Vm_l * (MINERAL - M_Lmin) / (km_l + MINERAL - M_Lmin) * t_scalar * w_scalar
        else
            f_MI_LM_des = 0.0_r8
        end if

        ! LMWC -> MINERAL: Adsorption/desorption function from Mayes 2012, SSAJ
        klmc_min = (10.0_r8 ** (-0.186_r8 * pH - 0.216_r8)) / 24.0_r8
        Qmax = bulkdensity * 1000.0_r8 * (silt + clay) * 100.0_r8 * par_pc            
        temp = (klmc_min * Qmax * LMWC ) / (2.0_r8 + klmc_min * LMWC) - MINERAL

        ! Equation 9 implementation
        f_LM_MI_sor = (temp / Qmax + 0.0015_r8) * LMWC / 50.0_r8 * t_scalar * w_scalar 

        if (f_LM_MI_sor < (LMWC * 0.9_r8)) then
            f_LM_MI_sor = f_LM_MI_sor 
        else
            f_LM_MI_sor = LMWC * 0.9_r8
        end if
            
        ! LMWC -> MB
        if (LMWC > 0._r8) then
            f_LM_MB_uptake = LMWC * klmc * t_scalar * w_scalar * MB / (MB + kes) * LMWC / (20.0_r8 + LMWC)
            temp2 = f_LM_MB_uptake * (1.0_r8 - (CUEref + CUET * (forc_st - Taeref)))
            if (temp2 < 0._r8) then
                temp2 = 0_r8
            end if
            f_LM_MB_uptake = f_LM_MB_uptake - temp2
        end if

        ! POM -> LMWC (Equation 2)
        if (POM > 0._r8) then
            f_PO_LM_dep = Vpom_lmc * POM / (POM + kpom) * t_scalar * w_scalar 
        end if

        if (f_PO_LM_dep > (0.9_r8 * POM)) then
            f_PO_LM_dep = 0.9_r8 * POM
        end if
            
        ! MB -> MINERAL    
        if (MB > 0._r8 .and. MINERAL < Qmax) then
            f_MB_MI_sor = MB * kmic * 0.15_r8 * t_scalar_mb * w_scalar  
        else
            f_MB_MI_sor = 0.0_r8
        end if
        
        if (f_MB_MI_sor > 0.9_r8 * MB) then
            f_MB_MI_sor = 0.9_r8 * MB
        end if
        if (f_MB_MI_sor < 0.0_r8) then
            f_MB_MI_sor = 0.0_r8
        end if
        
        ! MB -> ATM
        if (MB > 0._r8) then
            f_MB_atm = temp2 + MB * kmic * t_scalar_mb * w_scalar 
        end if
        
        ! POM -> SOILAGG (Equation 5)
        if (POM > 0._r8) then
            f_PO_SO_agg = Vpom_agg * POM / (kpom_agg + POM) * (1.0_r8 - SOILAGG / AGGmax) * t_scalar * w_scalar
        end if
        
        if (f_PO_SO_agg > 0.9_r8 * POM) then
            f_PO_SO_agg = 0.9_r8 * POM
        end if

        ! MINERAL -> SOILAGG (Equation 15)
        if (MINERAL > 0._r8) then
            f_MI_SO_agg = Vmin_agg * MINERAL / (kmin_agg + MINERAL) * (1.0_r8 - SOILAGG / AGGmax) 
        end if

        if (f_MI_SO_agg > 0.9_r8 * MINERAL) then
            f_MI_SO_agg = 0.9_r8 * MINERAL
        end if
        
        ! SOILAGG -> MINERAL (Equation 6)
        if (SOILAGG > 0._r8) then
            f_SO_break = SOILAGG * kagg * t_scalar * w_scalar
            f_SO_PO_break = f_SO_break * (1.0_r8 - pAG_MI)
            f_SO_MI_break = f_SO_break * pAG_MI
        end if

        if ((f_PO_LM_dep + f_PO_SO_agg) > POM) then
            temp3 = POM / (f_PO_LM_dep + f_PO_SO_agg)
            f_PO_LM_dep = f_PO_LM_dep * temp3
            f_PO_SO_agg = f_PO_SO_agg * temp3
        end if
        
        ! Update Carbon Pool Values
        LMWC    = LMWC + (f_PO_LM_dep + f_MI_LM_des - f_LM_leaching - f_LM_MI_sor - f_LM_MB_uptake - temp2) + forc_npp / 3.0_r8
        POM     = POM + (f_SO_PO_break - f_PO_LM_dep - f_PO_SO_agg) + forc_npp * 2.0_r8 / 3.0_r8
        MB      = MB + (f_LM_MB_uptake - f_MB_MI_sor - f_MB_atm)
        MINERAL = MINERAL + (f_LM_MI_sor + f_MB_MI_sor + f_SO_MI_break - f_MI_LM_des - f_MI_SO_agg)
        SOILAGG = SOILAGG + (f_PO_SO_agg + f_MI_SO_agg - f_SO_PO_break - f_SO_MI_break)
        
    end subroutine decomp

end module decomp_module