!>------------------------------------------------------------------------<!
!> Unit test module for state_m to excercise the state_t object           <!
!>------------------------------------------------------------------------<!
module test_unit_state
    use test_state_fixtures_m
    use fortuno_serial, only : test => serial_case_item, check => serial_check, test_list, is_close, is_equal
    use kinds_m
    use state_m, only : state_t
    implicit none
    private
    public :: tests


contains

    !>------------------------------------------------------------------------<!
    !> Collects the state tests as a returnable test_list                     <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([test("test_init_state", test_init_state)])
    end function tests


    !>------------------------------------------------------------------------<!
    !> Exercises initializing the state object                                <!
    !>------------------------------------------------------------------------<!
    subroutine test_init_state()
        type(test_env_t) :: env
        type(state_t) :: state
        call env%initialize_env_small_grid()
        call state%initialize(2, env%grid)
        call check(is_equal(state%n_vars, 2_ip))

        call check(allocated(state%u))

        call check(size(state%u, dim=1) == 2)
        call check(lbound(state%u, dim=2) == 1-env%grid%n_ghost)
        call check(ubound(state%u, dim=2) == env%grid%n_cells+env%grid%n_ghost)
    end subroutine test_init_state

end module test_unit_state