!>------------------------------------------------------------------------<!
!> Unit test module for flux_m, exercised directly on hand-picked         <!
!> left/right states -- no grid, no reconstruction, no time integration   <!
!> involved                                                               <!
!>------------------------------------------------------------------------<!
module test_unit_advection_flux
    use kinds_m, only: rp
    use fortuno_serial, only: test => serial_case_item, &
                              check => serial_check, test_list
    use flux_m, only: upwind_flux_advection
    implicit none
    private
    public :: tests

contains

    !>------------------------------------------------------------------------<!
    !> Collects the flux tests as a returnable test_list                      <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            test("upwind_picks_left_state_wahen_a_positive", test_upwind_picks_left_when_a_positive), &
            test("upwind_picks_right_state_when_a_negative", test_upwind_picks_right_when_a_negative), &
            test("upwind_consistency_uL_equals_uR", test_upwind_consistency), &
            test("upwind_flux_scales_linearly_with_a", test_upwind_scales_with_a), &
            test("upwind_flux_multivar_shape", test_upwind_flux_multivar_shape) &
        ])
    end function tests

    !>------------------------------------------------------------------------<!
    !> For a > 0, information travels left-to-right: the flux should use      <!
    !> ONLY the left state, regardless of what the right state is set to.     <!
    !>------------------------------------------------------------------------<!
    subroutine test_upwind_picks_left_when_a_positive()
        real(rp) :: uL(1), uR(1), a, F(1)
        uL = [2.0_rp]
        uR = [999.0_rp]   ! deliberately wrong-looking if this leaks into F
        a = 1.0_rp

        F = upwind_flux_advection(uL, uR, a)

        call check(F(1) == a * uL(1))
    end subroutine test_upwind_picks_left_when_a_positive

    !>------------------------------------------------------------------------<!
    !> For a < 0, the flux should use ONLY the right state.                   <!
    !>------------------------------------------------------------------------<!
    subroutine test_upwind_picks_right_when_a_negative()
        real(rp) :: uL(1), uR(1), a, F(1)
        uL = [999.0_rp]
        uR = [3.0_rp]
        a = -1.0_rp

        F = upwind_flux_advection(uL, uR, a)

        call check(F(1) == a * uR(1))
    end subroutine test_upwind_picks_right_when_a_negative

    !>------------------------------------------------------------------------<!
    !> Basic consistency property every numerical flux must satisfy:          <!
    !> if uL == uR (no discontinuity at all), the numerical flux must equal   <!
    !> the exact physical flux a*u, regardless of the sign of a.              <!
    !>------------------------------------------------------------------------<!
    subroutine test_upwind_consistency()
        real(rp) :: u(1), a, F(1)
        u = [4.0_rp]

        a = 1.0_rp
        F = upwind_flux_advection(u, u, a)
        call check(F(1) == a * u(1))

        a = -1.0_rp
        F = upwind_flux_advection(u, u, a)
        call check(F(1) == a * u(1))
    end subroutine test_upwind_consistency

    !>------------------------------------------------------------------------<!
    !> Flux should scale linearly with the advection speed for fixed states.  <!
    !>------------------------------------------------------------------------<!
    subroutine test_upwind_scales_with_a()
        real(rp) :: uL(1), uR(1), F1(1), F2(1)
        uL = [2.0_rp]
        uR = [5.0_rp]

        F1 = upwind_flux_advection(uL, uR, 1.0_rp)
        F2 = upwind_flux_advection(uL, uR, 2.0_rp)

        call check(F2(1) == 2.0_rp * F1(1))
    end subroutine test_upwind_scales_with_a

    !>------------------------------------------------------------------------<!
    !> Confirm the array-valued signature (length-n_vars in, length-n_vars    <!
    !> out) behaves correctly for n_vars > 1, independent per row             <!
    !>------------------------------------------------------------------------<!
    subroutine test_upwind_flux_multivar_shape()
        real(rp) :: uL(2), uR(2), a, F(2)
        uL = [1.0_rp, 10.0_rp]
        uR = [2.0_rp, 20.0_rp]
        a = 1.0_rp

        F = upwind_flux_advection(uL, uR, a)

        call check(F(1) == a * uL(1))
        call check(F(2) == a * uL(2))
    end subroutine test_upwind_flux_multivar_shape

end module test_unit_advection_flux
