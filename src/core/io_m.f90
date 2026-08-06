module io_m
    use kinds_m, only : rp, ip
    use grid_m, only : grid_t
    use state_m, only : state_t
    implicit none
    private
    public :: write_state_csv

contains

    subroutine write_state_csv(filename, grid, state)
        character(len=*), intent(in) :: filename
        type(grid_t), intent(in) :: grid
        type(state_t), intent(in) :: state

        integer :: ounit
        integer(ip) :: i

        open(newunit=ounit, file=filename, status="unknown")

        write(ounit, "(A)") "x,u"

        do i = 1,grid%n_cells
            write(ounit, "(1(E16.8,A),E16.8)") grid%xc(i), ",", state%u(1,i)
        end do

        close(ounit)

    end subroutine write_state_csv

end module io_m