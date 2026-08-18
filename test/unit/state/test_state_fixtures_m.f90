!>------------------------------------------------------------------<!
!> Unit test module for defining test fixtures                      <!
!>------------------------------------------------------------------<!
module test_state_fixtures_m
    use kinds_m, only: rp, ip
    use grid_m, only: grid_t
    implicit none
    private
    public :: test_env_t

    !>------------------------------------------------------------------<!
    !> Test environment object used to predefine grid and state with a  <!
    !> specific set of initial conditions.                              <!
    !>                                                                  <!
    !> Allows for consistency and reliability in tests                  <!
    !>------------------------------------------------------------------<!
    type test_env_t
        type(grid_t) :: grid
    contains
        procedure :: make_grid
        procedure :: initialize_env_small_grid
    end type

contains

    !>------------------------------------------------------------------<!
    !> Procedure to initialize a small test environment with the        <!
    !> following test conditions:                                       <!
    !>     n_cells = 10                                                 <!
    !>     n_ghost = 2                                                  <!
    !>     x_min = 0                                                    <!
    !>     x_max = 1                                                    <!
    !>                                                                  <!
    !> Sets up the grid object                                          <!
    !>------------------------------------------------------------------<!
    subroutine initialize_env_small_grid(this)
        class(test_env_t), intent(inout) :: this
        call this%make_grid(10_ip, 2_ip, 0.0_rp, 1.0_rp)
    end subroutine initialize_env_small_grid

    !>-------------------------------------------------------------------<!
    !> Builds a fresh grid_t for tests, called once per test case body   <!
    !>-------------------------------------------------------------------<!
    subroutine make_grid(this, n_cells, n_ghost, x_min, x_max)
        class(test_env_t), intent(inout) :: this
        integer(ip), intent(in) :: n_cells, n_ghost
        real(rp), intent(in) :: x_min, x_max
        call this%grid%initialize(n_cells, x_min, x_max, n_ghost)
    end subroutine make_grid

end module test_state_fixtures_m