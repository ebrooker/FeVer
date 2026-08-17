module boundary_conditions_m
    use kinds_m, only : rp
    use grid_m, only : grid_t
    use state_m, only : state_t
    implicit none
    private
    public :: fill_ghost_cells_outflow, fill_ghost_cells_periodic, bc_procedure_i
    public :: select_boundary_condition


    abstract interface
        subroutine bc_procedure_i(grid, state)
            import :: rp, grid_t, state_t
            type(grid_t), intent(in) :: grid
            type(state_t), intent(inout) :: state
        end subroutine
    end interface

contains

    function select_boundary_condition(bc_name) result(bc_ptr)
        character(len=*), intent(in) :: bc_name
        procedure(bc_procedure_i), pointer :: bc_ptr
        select case (trim(bc_name))
        case ("periodic")
            bc_ptr => fill_ghost_cells_periodic
        case ("outflow")
            bc_ptr => fill_ghost_cells_outflow
        case default
            error stop "Unknown boundary condition: " // trim(bc_name)
        end select
    end function select_boundary_condition

    subroutine fill_ghost_cells_periodic(grid, state)
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

    end subroutine fill_ghost_cells_periodic


    subroutine fill_ghost_cells_outflow(grid, state)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(inout) :: state

        !! Fill ghost cells on LHS with first real mesh point (i=1)
        state%u(:,grid%ilo:0) = spread(state%u(:,1), dim=2, ncopies=grid%n_ghost)

        !! Fill ghost cells on RHS with last real mesh point (i=n_cells)
        state%u(:,grid%n_cells+1:) = spread(state%u(:,grid%n_cells), dim=2, ncopies=grid%n_ghost)

    end subroutine fill_ghost_cells_outflow


end module boundary_conditions_m