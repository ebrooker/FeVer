module boundary_conditions_m
    use kinds_m, only : rp
    use grid_m, only : grid_t
    use state_m, only : state_t
    implicit none
    private
    public :: apply_periodic_bc

contains

    subroutine apply_periodic_bc(grid, state)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(inout) :: state

        integer :: i

        !! Fill ghost cells on LHS
        do i=1-grid%n_ghost,0
            state%u(:,i) = state%u(:,grid%n_cells + i)
        end do

        !! Fill ghost cells on RHS
        do i = grid%n_cells + 1, grid%n_cells + grid%n_ghost
            state%u(:,i) = state%u(:,i - grid%n_cells)
        end do

        !! Ensure boundary conditions were correctly set
        ! if (state%u(1,0) /= state%u(1,grid%n_cells)) stop "u(0) /= u(n_cells)"
        ! if (state%u(1,grid%n_cells+1) /= state%u(1,1)) stop "u(n_cells+1) /= u(1)"

    end subroutine apply_periodic_bc

end module boundary_conditions_m