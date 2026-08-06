!>------------------------------------------------------------------------<!
!> Unit test module for time_integration_m to excercise the grid_t object <!
!>------------------------------------------------------------------------<!
module test_unit_time_integration
    use kinds_m, only: rp, ip
    use fortuno_serial, only: test => serial_case_item, &
                              check => serial_check, test_list
    use time_integration_m, only: compute_dt
    implicit none
    private
    public :: tests

    real(rp), parameter :: pi = 4.0_rp * atan(1.0_rp)

contains

    !>------------------------------------------------------------------------<!
    !> Collects the time integration tests as a returnable test_list          <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            test("dt_matches_cfl_formula", test_dt_matches_cfl_formula) &
        ])
    end function tests

    !>------------------------------------------------------------------------<!
    !> Test compute_dt to confirm the formula directly                        <!
    !>------------------------------------------------------------------------<!
    subroutine test_dt_matches_cfl_formula()
        real(rp) :: dx, a, cfl, dt
        dx = 0.1_rp
        a = 2.0_rp
        cfl = 0.5_rp

        dt = compute_dt(dx, a, cfl)

        call check(dt == cfl * dx / abs(a))
    end subroutine test_dt_matches_cfl_formula

end module test_unit_time_integration