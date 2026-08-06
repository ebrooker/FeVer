module time_integration_m
    use reconstruct_m, only : reconstruct_constant
    use kinds_m, only : rp, ip
    use flux_m, only : upwind_flux_advection
    use grid_m, only : grid_t
    use state_m, only : state_t
    implicit none
    private
    public :: compute_dt, advance_euler_explicit


contains

    !!
    !! Ghost cells must be filled prior to call
    !!
    !! TODO: Guard against unfilled ghost cells
    !!
    subroutine advance_euler_explicit(grid, state, dt, a)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(inout) :: state
        real(rp), intent(in) :: dt, a

        !! RHS update
        real(rp), allocatable :: dudt(:,:)
        if (allocated(dudt)) deallocate(dudt)
        allocate(dudt(state%n_vars,1:grid%n_cells))

        call compute_rhs(grid, state%u, a, dudt)

        !! Update solution with timestep integrated RHS
        state%u(:,1:grid%n_cells) = state%u(:,1:grid%n_cells) + dt * dudt

        if (allocated(dudt)) deallocate(dudt)

    end subroutine advance_euler_explicit

    subroutine compute_rhs(grid, u, speed, dudt)
        type(grid_t), intent(in) :: grid
        real(rp), intent(in) :: u(:,:)
        real(rp), intent(in) :: speed
        real(rp), intent(out) :: dudt(:,:)

        real(rp), dimension(size(u,dim=1),1-grid%n_ghost:grid%n_cells+grid%n_ghost) :: u_left, u_right
        integer(ip) :: i

        call reconstruct_constant(u, u_left, u_right)

        do i = lbound(dudt,dim=2), ubound(dudt,dim=2)
            dudt(:,i) = upwind_flux_advection(u_left(:,i-1), u_right(:,i), speed) &
                      - upwind_flux_advection(u_left(:,i), u_right(:,i+1), speed)
        end do
        dudt = dudt / grid%dx
    end subroutine compute_rhs

    pure function compute_dt(dx, a, cfl) result(dt)
        real(rp), intent(in) :: dx, a, cfl
        real(rp) :: dt
        dt = CFL * dx / abs(a)
    end function compute_dt

end module time_integration_m