module time_integration_m
    use reconstruct_m, only : reconstruct_constant, reconstruct_linear_minmod
    use kinds_m, only : rp, ip
    use flux_m, only : upwind_flux_advection
    use grid_m, only : grid_t
    use state_m, only : state_t
    use boundary_conditions_m, only : apply_periodic_bc
    implicit none
    private
    public :: compute_dt, advance_euler_explicit


contains

    !!
    !! Ghost cells must be filled prior to call
    !!
    !! TODO: Guard against unfilled ghost cells
    !!
    subroutine advance_euler_explicit(grid, state, dt, a, reconstruction_method)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(inout) :: state
        real(rp), intent(in) :: dt, a
        character(len=*), intent(in) :: reconstruction_method

        !! RHS update
        real(rp), allocatable :: dudt(:,:)
        if (allocated(dudt)) deallocate(dudt)
        allocate(dudt(state%n_vars,1:grid%n_cells))


        !! Select the interface reconstruction method
        call compute_rhs(grid, state%u, a, dudt, dt, reconstruction_method)
        select case(trim(reconstruction_method))
            case ("constant")
                state%u(:,1:grid%n_cells) = state%u(:,1:grid%n_cells) + dt * dudt
            case ("linear-minmod")
                block
                    real(rp), allocatable :: utmp(:,:)
                    allocate(utmp(state%n_vars, grid%ilo:grid%ihi))
                    utmp(:,1:grid%n_cells) = state%u(:,1:grid%n_cells)
                    state%u(:,1:grid%n_cells) = state%u(:,1:grid%n_cells) + 0.5 * dt * dudt
                    call apply_periodic_bc(grid, state)
                    call compute_rhs(grid, state%u, a, dudt, dt, reconstruction_method)
                    state%u(:,1:grid%n_cells) = utmp(:,1:grid%n_cells) + dt * dudt
                end block
            case default
        end select

        if (allocated(dudt)) deallocate(dudt)

    end subroutine advance_euler_explicit

    subroutine compute_rhs(grid, u, speed, dudt, dt, reconstruction_method)
        type(grid_t), intent(in) :: grid
        real(rp), allocatable, intent(in) :: u(:,:)
        real(rp), intent(in) :: speed, dt
        real(rp), intent(out) :: dudt(:,:)
        character(len=*), intent(in) :: reconstruction_method

        real(rp), allocatable :: u_left(:,:), u_right(:,:)
        integer(ip) :: i

        if (allocated(u_left)) deallocate(u_left)
        allocate(u_left(size(u,dim=1),1-grid%n_ghost:grid%n_cells+grid%n_ghost), source=0.0_rp)

        if (allocated(u_right)) deallocate(u_right)
        allocate(u_right(size(u,dim=1),1-grid%n_ghost:grid%n_cells+grid%n_ghost), source=0.0_rp)


        !! Select the interface reconstruction method
        select case(trim(reconstruction_method))
            case ("constant")
                call reconstruct_constant(u, u_left, u_right)
            case ("linear-minmod")
                call reconstruct_linear_minmod(u,u_left,u_right,1,grid%n_cells)
            case default
                call reconstruct_linear_minmod(u,u_left,u_right,1,grid%n_cells)
        end select

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