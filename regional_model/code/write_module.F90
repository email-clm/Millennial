! # This is the write module for the regional model. It contains the subroutine that writes output data to a file.
! # History: June 2024, Xiaofeng Xu, seperated this from the main program of the Millennial model
! # The writeoutput subroutine writes output data to a file.

module write_mod
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
        integer            :: i, ier, file_unit

        ! (The rest of your working subroutine code goes here...)
        write(year_str, '(I0)') year
        outputfile = trim(path_output) // "/" // trim(string) // trim(year_str) // ".txt"
        
        open(newunit=file_unit, file=trim(outputfile), status='REPLACE', action='write', iostat=ier)
        if (ier == 0) then
            do i = 1, size(array)
                write(file_unit, *) array(i)
            end do
            close(file_unit)
        end if
    end subroutine writeoutput

end module write_mod
