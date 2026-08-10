module io_m
    use kinds_m, only : rp, ip
    use grid_m, only : grid_t
    use state_m, only : state_t
    implicit none
    private
    public :: write_state_csv, snapshot_filename

contains

    function snapshot_filename(base_name, snap) result(filename)
        character(len=*), intent(in) :: base_name
        integer, intent(in) :: snap
        character(len=:), allocatable :: filename
        character(len=256) :: buf

        write(buf, '(A,"_",I0.5)') trim(base_name), snap
        filename = trim(buf)
    end function snapshot_filename

    subroutine write_state_csv(filename, grid, state)
        character(len=*), intent(in) :: filename
        type(grid_t), intent(in) :: grid
        type(state_t), intent(in) :: state

        integer :: ounit
        integer(ip) :: i, n
        character(len=:), allocatable :: filename_csv
        filename_csv = trim(filename // ".csv")

        open(newunit=ounit, file=filename_csv, status="unknown")

        write(ounit, "(A)") "x,u"

        do i = 1,grid%n_cells
            write(ounit, "(E16.8)", advance="no") grid%xc(i)
            do n = 1,state%n_vars
                write(ounit,"(A,E16.8)",advance="no") ",", state%u(n,i)
            end do
            write(ounit,*)
        end do

        close(ounit)

    end subroutine write_state_csv

end module io_m