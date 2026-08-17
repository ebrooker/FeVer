!>------------------------------------------------------------------<!
!> Unit test module for testing boundary_conditions_m               <!
!>------------------------------------------------------------------<!
module test_unit_boundary_conditions
    use kinds_m
    use boundary_conditions_m, only: fill_ghost_cells_outflow, fill_ghost_cells_periodic
    use test_unit_fixtures, only : test_env_t
    use fortuno_serial, only: is_equal, test => serial_case_item, check => serial_check, test_list
    implicit none
    private

    public :: tests, test_periodic_bcs

contains

    !>------------------------------------------------------------------<!
    !> Collects the boundary condition tests as a returnable test_list  <!
    !>------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            test("Periodic BCs", test_periodic_bcs) &
        ])
    end function tests

    !>------------------------------------------------------------------<!
    !> Tests periodic boundary conditions with the following conditions <!
    !>     n_cells=10                                                   <!
    !>     n_ghost=2                                                    <!
    !>     x_min=0                                                      <!
    !>     x_max=1                                                      <!
    !>                                                                  <!    
    !> test_env_t is initialized to create grid and state objects with  <!
    !> the above defined conditions                                     <!
    !>------------------------------------------------------------------<!
    subroutine test_periodic_bcs()
        type(test_env_t) :: test_env
        call test_env%initialize_env_small()
        call test_env%set_sine_wave()
        call fill_ghost_cells_periodic(test_env%grid, test_env%state)

        !! Check left hand side
        call check( test_env%state%u(1,0)  == test_env%state%u(1,test_env%grid%n_cells))
        call check( test_env%state%u(1,-1) == test_env%state%u(1,test_env%grid%n_cells-1))

        !! Check right hand side
        call check( test_env%state%u(1,test_env%grid%n_cells+1) == test_env%state%u(1,1) )
        call check( test_env%state%u(1,test_env%grid%n_cells+2) == test_env%state%u(1,2) )

    end subroutine test_periodic_bcs

end module test_unit_boundary_conditions