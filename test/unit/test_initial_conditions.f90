!>------------------------------------------------------------------------<!
!> Unit test module for functions_1d_m to excercise the grid_t object     <!
!>------------------------------------------------------------------------<!
module test_unit_initial_conditions
    use kinds_m
    use functions_1d_m, only: sine, square_pulse
    use fortuno_serial, only: is_equal, test => serial_case_item, check => serial_check, test_list
    implicit none

contains

    !>------------------------------------------------------------------------<!
    !> Collects the ICs tests as a returnable test_list                       <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        ! cases: sine at known phase points (0, pi/2, pi), square pulse inside/outside/at edge
        tests = test_list([test("square pulse wave", test_square_pulse)])
    end function tests

    !>------------------------------------------------------------------------<!
    !> Test setting a square pulse wave with hat in range [3.0,7.0]           <!
    !>------------------------------------------------------------------------<!
    subroutine test_square_pulse()
        real(rp) :: u(11),x(11)
        x = [0,1,2,3,4,5,6,7,8,9,10]
        u = square_pulse(x,3.0_rp,7.0_rp)
        call check(all(u(:4) == 0.0_rp))
        call check(all(u(5:7) == 1.0_rp))
        call check(all(u(8:) == 0.0_rp))
    end subroutine test_square_pulse

end module test_unit_initial_conditions