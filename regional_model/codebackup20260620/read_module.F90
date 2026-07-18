module read_mod
    implicit none
contains

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

end module read_mod
