!	main program starts
	PROGRAM Millennial
!	History
!	Xiaofeng Xu created this code program to play with Millennial model structure (ICOS workshop Mar 14-16, 2016 in Boulder, CO)
!	The code is created in May - June 2016, solely by Xiaofeng XU (xxu@sdsu.edu)
!	Jan 2021, the code has been cleaned to close the three issues identified when Benjamin Bond-Lamberty's team converted to R script; the century model code has been removed.
!	This is a toy version of the Millennial model (C-only version, N P will be added in future updates)

!   Commented out model output for Century framework, added daily outputs for all processes Nov 7, 2024 - X Xu

!   Rose Abramoff modified the equations here to create Millennial V2, described in Abramoff et al. (2022); Xiaofeng brought in the edits by Rose on Nov 15, 2024

!	Xiaofeng Xu modified the code to add the regional capacility to allow the model to be set up for global simulation, with biome-specific paraemeters. June 10, 2026

	use read_mod
	use hydrology_mod
	use decomp_module
	use write_mod

	implicit none
	integer,parameter 	:: r8 = selected_real_kind(12) 	!8 byte real

    integer :: year, month, day, doy		! year, month, and day for the model simulations		
    integer :: startyear, endyear			! start year and end year for the mdoel simulations
    integer :: days_in_month(12)			! Array to hold the number of days in each month (January to December)
    integer :: max_days						! Variable to hold the maximum number of days for the current month
	integer :: datayear						! if one year is availabe for simulation, this is the year

	integer :: ngrid						! number of grids over the space (#)
	integer :: nr							! number of model steps (day)
	integer :: i							! integer for loop 
	integer :: j							! integer for loop
	integer :: n							! integer for loop
	integer :: ID							! integer for loop
	integer :: flag_initialfile				! flag initial file or not
	integer :: flag_output					! flag saving model output or not
	integer :: flag_annual					! flag saving model output as annual or daily
	integer :: flag_data_year				! flag for data year, 0 if you have one year data for model simulation, 1 if you have long term data

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
!    nr = int((endyear - startyear + 1) * 365)
	nr = 365		! keep this as one year to reduce the memory usage, the model will be operated at yearly basiss, and reading and writing will be finished for each year. 
    !allocate(filename(nr))

	write(*,*) "Please enter the number for total grids for the regional simulation (including NA):"
	read(*,*) ngrid

	write(*,*) "Please enter the name of the veg mask and soil bd, clay, silt file:"
	read(*,*) maskvegsoilfile

	write(*,*) "Please enter the name of the soil parameter file:"
	read(*,*) soilparafile

	write(*,*) "Do you have long term data for model simulation 1, if you have one year data for model simulation, 0"
	read(*,*) flag_data_year

	if(flag_data_year == 0) then
		write(*,*) "Please enter the year for your forcing data (e.g., 2010):"
		read(*,*) datayear
	end if

	write(*,*) "Please enter the folder path for soil temperature forcing data:"
	read '(A)', folder_path_st

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

	allocate(yLMWC(1:(endyear-startyear+1), 1:ngrid))
	allocate(yPOM(1:(endyear-startyear+1), 1:ngrid))
	allocate(yMB(1:(endyear-startyear+1), 1:ngrid))
	allocate(yMINERAL(1:(endyear-startyear+1), 1:ngrid))
	allocate(ySOILAGG(1:(endyear-startyear+1), 1:ngrid))
	
	allocate(yf_LM_leaching(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_MI_LM_des(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_LM_MI_sor(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_LM_MB_uptake(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_PO_LM_dep(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_MB_MI_sor(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_PO_SO_agg(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_MI_SO_agg(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_SO_PO_break(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_SO_MI_break(1:(endyear-startyear+1), 1:ngrid))
	allocate(yf_MB_atm(1:(endyear-startyear+1), 1:ngrid))

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

	write(*,*) 'Attempting to read veg, mask, and soil parameters .....'
	open(unit = 10, file=maskvegsoilfile, action='read', iostat=ier)
	if (ier /= 0) then
        print *, "Error: Could not open mas, veg, and soil property file!"
        stop
    end if
	!print *, "here 3", maskvegsoilfile
	do i = 1, ngrid
			read (10,*,iostat=ier) mask(i), bulkdensity(i), clay(i), silt(i), organic(i)
		if (ier /= 0) then
			write(*,*)'Error in reading in maskvegsoilfile'
		end if
	end do
	close(10)

	write(*,*) 'Attempting to read soil parameters .....'
	open(unit = 10, file=soilparafile)
	do i = 1, nbiome
		read (10,*,iostat=ier) (soil_par(i, j), j = 1, soil_par_num)
		if (ier /= 0) then
			write(*,*)'soilpara: error in reading in soilpara_in'
		end if
		
	!	Assign values read from soil parameter file to variables with names that match the
	!	names in the soil parameter file. The order of the parameters in the soil parameter file should be the same as the order of the variables here.
		j = 1
		maxpsi(i)				= soil_par(i,j); j = j + 1
		vwcsat(i)				= soil_par(i,j); j = j + 1
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
	read (11,*,iostat=ier) (initial_pom(i), i =1, ngrid)

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
	do year = startyear, endyear
	i = 1
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
                if(flag_data_year == 1) then
				write(filename(i), '(I4.4, I2.2, I2.2, A4)') &
                   year, month, day, ".txt"
				else
				write(filename(i), '(I4.4, I2.2, I2.2, A4)') &
                   datayear, month, day, ".txt"
				end if	               
                i = i + 1                
            end do ! Day loop
        end do ! Month loop

	! read int model forcing data by year
	print *, 'read forcing data of ', year
	do doy = 1, 365
		print *, 'read forcing data for day ', doy
		force_st_file(doy) = trim(folder_path_st) // trim(filename(doy))
	!	write(*,*) "reading soil temperature forcing data for day ", doy, ": ", force_st_file(doy)
		call readdata(force_st_file(doy), forc_st((doy),1:ngrid), ngrid)
		force_sw_file(doy) = trim(folder_path_sw) // trim(filename(doy))
	!	write(*,*) "reading soil moisture forcing data for day ", doy, ": ", force_sw_file(doy)
		call readdata(force_sw_file(doy), forc_sw((doy),1:ngrid), ngrid)
		force_npp_file(doy) = trim(folder_path_npp) // trim(filename(doy))
		call readdata(force_npp_file(doy), forc_npp((doy),1:ngrid), ngrid)
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
	case (112)
		ID = 12
	case (113)
		ID = 13
	case (114)
		ID = 14
	case (115)
		ID = 15
	case default
		continue
	!	print *, "Error: Mask value does not match any biome ID. Skipping this grid."
	end select

	do doy = 1, 365
	call soilpsi(bulkdensity(n), clay(n), silt(n), forc_sw((doy),n), vwcsat(ID), organic(n), psisat(n), psi(n), smp_l(n))

	if (year /= startyear .and. doy == 1) then
	LMWC(1, n) = LMWC(365, n)
	POM(1, n)=POM(365, n)
	MB(1, n)=MB(365, n)
	MINERAL(1, n)=MINERAL(365, n)
	SOILAGG(1, n)=SOILAGG(365, n)
	endif

	call decomp(soil_par(ID, 1:nbiome), forc_st((doy),n), forc_sw((doy),n), psi(n), forc_npp((doy),n), &
		bulkdensity(n), silt(n), clay(n), LMWC(doy, n), POM(doy, n), MB(doy, n), MINERAL(doy, n), &
		SOILAGG(doy, n), f_LM_leaching(doy, n), f_MI_LM_des(doy, n),f_LM_MI_sor(doy, n), &
		f_LM_MB_uptake(doy, n),f_PO_LM_dep(doy, n), f_MB_MI_sor(doy, n), f_PO_SO_agg(doy, n), &
		f_MI_SO_agg(doy, n), f_SO_PO_break(doy, n), f_SO_MI_break(doy, n),f_MB_atm(doy, n))

	yLMWC(year - startyear + 1, n) 		= yLMWC(year - startyear + 1, n) + LMWC(doy, n) / 365.0
	yPOM(year - startyear + 1, n) 		= yPOM(year - startyear + 1, n) + POM(doy, n) / 365.0
	yMB(year - startyear + 1, n) 		= yMB(year - startyear + 1, n) + MB(doy, n) / 365.0
	yMINERAL(year - startyear + 1, n) 	= yMINERAL(year - startyear + 1, n) + MINERAL(doy, n) / 365.0
	ySOILAGG(year - startyear + 1, n) 	= ySOILAGG(year - startyear + 1, n) + SOILAGG(doy, n) / 365.0

	yf_LM_leaching(year - startyear + 1, n) 	= yf_LM_leaching(year - startyear + 1, n) + f_LM_leaching(doy, n) / 365.0
	yf_MI_LM_des(year - startyear + 1, n) 		= yf_MI_LM_des(year - startyear + 1, n) + f_MI_LM_des(doy, n) / 365.0
	yf_LM_MI_sor(year - startyear + 1, n) 		= yf_LM_MI_sor(year - startyear + 1, n) + f_LM_MI_sor(doy, n) / 365.0
	yf_LM_MB_uptake(year - startyear + 1, n) 	= yf_LM_MB_uptake(year - startyear + 1, n) + f_LM_MB_uptake(doy, n) / 365.0
	yf_PO_LM_dep(year - startyear + 1, n) 		= yf_PO_LM_dep(year - startyear + 1, n) + f_PO_LM_dep(doy, n) / 365.0
	yf_MB_MI_sor(year - startyear + 1, n) 		= yf_MB_MI_sor(year - startyear + 1, n) + f_MB_MI_sor(doy, n) / 365.0
	yf_PO_SO_agg(year - startyear + 1, n) 		= yf_PO_SO_agg(year - startyear + 1, n) + f_PO_SO_agg(doy, n) / 365.0
	yf_MI_SO_agg(year - startyear + 1, n) 		= yf_MI_SO_agg(year - startyear + 1, n) + f_MI_SO_agg(doy, n) / 365.0
	yf_SO_PO_break(year - startyear + 1, n) 	= yf_SO_PO_break(year - startyear + 1, n) + f_SO_PO_break(doy, n) / 365.0
	yf_SO_MI_break(year - startyear + 1, n) 	= yf_SO_MI_break(year - startyear + 1, n) + f_SO_MI_break(doy, n) / 365.0
	yf_MB_atm(year - startyear + 1, n) 			= yf_MB_atm(year - startyear + 1, n) + f_MB_atm(doy, n) / 365.0

	!	update the pools after each iteration 
	if(doy < nr) then
	LMWC(doy+1, n)=LMWC(doy, n)
	POM(doy+1, n)=POM(doy, n)
	MB(doy+1, n)=MB(doy, n)
	MINERAL(doy+1, n)=MINERAL(doy, n)
	SOILAGG(doy+1, n)=SOILAGG(doy, n)
	endif

	end do	!	doy loop
	end if	!	mask check
end do		!	grid loop

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
