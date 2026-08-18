!>------------------------------------------------------------------<!
!> Unit test module for defining test fixtures                      <!
!>------------------------------------------------------------------<!
module test_bc_fixtures_m
    use kinds_m, only: rp, ip
    use grid_m, only: grid_t
    use state_m, only: state_t
    use functions_1d_m, only : square_pulse, sine
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
        type(state_t) :: state
    contains
        procedure :: make_grid, make_state
        procedure :: set_sine_wave, set_square_pulse_wave
        procedure :: initialize_env_small, initialize_env_small_grid
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
    !> Sets up the grid and state objects                               <!
    !>------------------------------------------------------------------<!
    subroutine initialize_env_small(this)
        class(test_env_t), intent(inout) :: this
        call this%initialize_env_small_grid
        call this%make_state(1_ip)
    end subroutine initialize_env_small

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

    !>-------------------------------------------------------------------<!
    !> Builds a fresh state_t for tests, called once per test case body  <!
    !>-------------------------------------------------------------------<!
    subroutine make_state(this, n_vars)
        class(test_env_t), intent(inout) :: this
        integer(ip), intent(in) :: n_vars
        call this%state%initialize(n_vars, this%grid)
    end subroutine make_state

    !>-------------------------------------------------------------------<!
    !> Sets initial condition for state_t%u to a sine wave using grid_t  <!
    !>-------------------------------------------------------------------<!
    subroutine set_sine_wave(this)
        class(test_env_t), intent(inout) :: this
        integer :: nvar
        do nvar = 1,this%state%n_vars
            this%state%u(nvar,1:this%grid%n_cells) = sine(this%grid%xc(1:this%grid%n_cells))
        end do
    end subroutine set_sine_wave

    !>-------------------------------------------------------------------<!
    !> Sets initial condition for state_t%u to a square pulse wave using <!
    !> grid_t                                                            <!
    !>-------------------------------------------------------------------<!
    subroutine set_square_pulse_wave(this, x_lo, x_hi)
        class(test_env_t), intent(inout) :: this
        real(rp), intent(in) :: x_lo, x_hi
        integer :: nvar
        do nvar = 1,this%state%n_vars
            this%state%u(nvar,1:this%grid%n_cells) = square_pulse(this%grid%xc(1:this%grid%n_cells), x_lo, x_hi)
        end do
    end subroutine set_square_pulse_wave


end module test_bc_fixtures_m