module time_integration_m
    use reconstruct_m, only : reconstruction_procedure_i
    use kinds_m, only : rp, ip
    use flux_m, only : upwind_flux_advection
    use grid_m, only : grid_t
    use state_m, only : state_t
    use boundary_conditions_m, only : bc_procedure_i
    implicit none
    private
    public :: compute_dt, advance_forward_euler, compute_rhs_advection, compute_dt_burgers
    public :: advance_rk2, rhs_procedure_i, integrator_procedure_i, select_integrator_method, select_rhs_method

    abstract interface
        function rhs_procedure_i(g, u, a, reconstruction) result(dudt)
            import :: rp, grid_t
            type(grid_t), intent(in) :: g
            real(rp), intent(in), allocatable :: u(:,:)
            real(rp), intent(in) :: a
            procedure(reconstruction_procedure_i) :: reconstruction
            real(rp) :: dudt(size(u,1), size(u,2))
        end function
    end interface

    abstract interface
        subroutine integrator_procedure_i(grid, state, dt, a, rhs, fill_ghost_cells, reconstruction)
            import rp, grid_t, state_t
            type(grid_t), intent(in) :: grid
            type(state_t), intent(inout) :: state
            real(rp), intent(in) :: dt, a
            procedure(rhs_procedure_i) :: rhs
            procedure(bc_procedure_i) :: fill_ghost_cells
            procedure(reconstruction_procedure_i) :: reconstruction
        end subroutine integrator_procedure_i
    end interface


contains

        !!!> TODO: Create abstract interface for the solution advancing methods
        !!!> TODO: 

    function select_rhs_method(rhs_name) result(rhs_ptr)
        character(len=*), intent(in) :: rhs_name
        procedure(rhs_procedure_i), pointer :: rhs_ptr
        select case (trim(rhs_name))
        case ("advection")
            rhs_ptr => compute_rhs_advection
        case default
            error stop "Unknown rhs: " // trim(rhs_name)
        end select
    end function select_rhs_method



    function select_integrator_method(integrator_name) result(integrator_ptr)
        character(len=*), intent(in) :: integrator_name
        procedure(integrator_procedure_i), pointer :: integrator_ptr
        select case (trim(integrator_name))
        case ("forward-euler")
            integrator_ptr => advance_forward_euler
        case ("rk2")
            integrator_ptr => advance_rk2
        case default
            error stop "Unknown integrator: " // trim(integrator_name)
        end select
    end function select_integrator_method



    subroutine advance_forward_euler(grid, state, dt, a, rhs, fill_ghost_cells, reconstruction)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(inout) :: state
        real(rp), intent(in) :: dt, a
        procedure(rhs_procedure_i) :: rhs
        procedure(bc_procedure_i) :: fill_ghost_cells
        procedure(reconstruction_procedure_i) :: reconstruction

        call fill_ghost_cells(grid, state%u)
        state%u(:,1:grid%n_cells) = state%u(:,1:grid%n_cells) + dt * rhs(grid, state%u, a, reconstruction)

    end subroutine advance_forward_euler

    
    subroutine advance_rk2(grid, state, dt, a, rhs, fill_ghost_cells, reconstruction)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(inout) :: state
        real(rp), intent(in) :: dt, a
        procedure(rhs_procedure_i) :: rhs
        procedure(bc_procedure_i) :: fill_ghost_cells
        procedure(reconstruction_procedure_i) :: reconstruction
        real(rp), allocatable :: u_tmp(:,:)

        if (allocated(u_tmp)) deallocate(u_tmp)
        allocate(u_tmp(state%n_vars, grid%ilo:grid%ihi))

        call fill_ghost_cells(grid, state%u)
        
        !! Compute half timestep
        u_tmp(:,1:grid%n_cells) = state%u(:,1:grid%n_cells) + 0.5 * dt * rhs(grid, state%u, a, reconstruction)

        !! Finish timestep with fluxes from half dt solution and initial state copy
        call fill_ghost_cells(grid, u_tmp)
        state%u(:,1:grid%n_cells) = state%u(:,1:grid%n_cells) + dt * rhs(grid, u_tmp, a, reconstruction)

    end subroutine advance_rk2

    function compute_rhs_advection(grid, u, speed, reconstruction) result(dudt)
        type(grid_t), intent(in) :: grid
        real(rp), allocatable, intent(in) :: u(:,:)
        real(rp), intent(in) :: speed
        procedure(reconstruction_procedure_i) :: reconstruction
        real(rp) :: dudt(size(u,dim=1),1:grid%n_cells)
        
        real(rp), allocatable :: u_left(:,:), u_right(:,:)
        integer(ip) :: i

        if (allocated(u_left)) deallocate(u_left)
        allocate(u_left(size(u,dim=1),1-grid%n_ghost:grid%n_cells+grid%n_ghost), source=0.0_rp)

        if (allocated(u_right)) deallocate(u_right)
        allocate(u_right(size(u,dim=1),1-grid%n_ghost:grid%n_cells+grid%n_ghost), source=0.0_rp)

        call reconstruction(u, u_left, u_right)

        do i = lbound(dudt,dim=2), ubound(dudt,dim=2)
            dudt(:,i) = upwind_flux_advection(u_left(:,i-1), u_right(:,i), speed) &
                      - upwind_flux_advection(u_left(:,i), u_right(:,i+1), speed)
        end do
        dudt = dudt / grid%dx
    end function compute_rhs_advection

    pure function compute_dt(dx, a, cfl) result(dt)
        real(rp), intent(in) :: dx, a, cfl
        real(rp) :: dt
        dt = CFL * dx / abs(a)
    end function compute_dt

    pure function compute_dt_burgers(grid, state, cfl) result(dt)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(in) :: state
        real(rp), intent(in) :: cfl
        real(rp) :: dt
        dt = cfl * grid%dx / maxval(abs(state%u))
    end function compute_dt_burgers

end module time_integration_m