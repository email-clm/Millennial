! # This is the hydrology module for the regional model. It contains the subroutine that calculates soil PSI!!
! # History: June 2024, Xiaofeng Xu, seperated this from the main program of the Millennial model
! # The soil PSI subroutine calculates the soil matric potential based on the Community Land Model (CLM) formulation.

module hydrology_mod
    implicit none
contains

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

end module hydrology_mod

