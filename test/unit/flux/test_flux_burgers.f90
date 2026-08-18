!>------------------------------------------------------------------------<!
!> Unit test module for flux_m, exercised directly on hand-picked         <!
!> left/right states -- no grid, no reconstruction, no time integration   <!
!> involved                                                               <!
!>------------------------------------------------------------------------<!
module test_unit_burgers_flux
    use kinds_m, only: rp
    use fortuno_serial, only: test => serial_case_item, &
                              check => serial_check, test_list
    use burgers_flux_m, only: rusanov_flux_burgers, godunov_flux_burgers, burgers_physical_flux
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
            test("physical_flux_matches_formula", test_physical_flux), &
            test("rusanov_consistency_all_signs", test_rusanov_consistency), &
            test("godunov_consistency_all_signs", test_godunov_consistency), &
            test("rusanov_shock_hand_computed", test_rusanov_shock_hand_computed), &
            test("godunov_shock_hand_computed", test_godunov_shock_hand_computed), &
            test("godunov_right_moving_rarefaction", test_godunov_right_moving), &
            test("godunov_left_moving_rarefaction", test_godunov_left_moving), &
            test("godunov_transonic_rarefaction", test_godunov_transonic), &
            test("rusanov_transonic_documented_mismatch", test_rusanov_transonic_sanity) &
        ])
    end function tests

    !>------------------------------------------------------------------------<!
    !> f(u) = u^2/2, directly. Include a negative and a zero case -- f is     <!
    !> even, worth confirming that isn't accidentally lost (e.g. via abs()    <!
    !> applied somewhere it shouldn't be).                                    <!
    !>------------------------------------------------------------------------<!
    subroutine test_physical_flux()
        real(rp) :: u(1), f(1)

        u = [2.0_rp]
        f = burgers_physical_flux(u)
        call check(f(1) == 2.0_rp)

        u = [-3.0_rp]
        f = burgers_physical_flux(u)
        call check(f(1) == 4.5_rp)

        u = [0.0_rp]
        f = burgers_physical_flux(u)
        call check(f(1) == 0.0_rp)
    end subroutine test_physical_flux

    !>------------------------------------------------------------------------<!
    ! Every numerical flux must satisfy F(u,u) = f(u), regardless of sign --
    ! the same property checked for upwind_flux_advection in Phase 1, and
    ! the property Phase 5's rusanov_flux(U,U) will need for the vector
    ! case. Sweep positive, negative, and zero u so a sign-dependent bug
    ! can't hide.
    !>------------------------------------------------------------------------<!
    subroutine test_rusanov_consistency()
        real(rp) :: test_values(5), u(1), f(1), f_exact(1)
        integer :: i
        test_values = [-2.0_rp, -0.5_rp, 0.0_rp, 1.0_rp, 3.0_rp]
        do i = 1, size(test_values)
            u = [test_values(i)]
            f = rusanov_flux_burgers(u, u)
            f_exact = burgers_physical_flux(u)
            call check(f(1) == f_exact(1))
        end do
    end subroutine test_rusanov_consistency

    subroutine test_godunov_consistency()
        real(rp) :: test_values(5), u(1), f(1), f_exact(1)
        integer :: i
        test_values = [-2.0_rp, -0.5_rp, 0.0_rp, 1.0_rp, 3.0_rp]
        do i = 1, size(test_values)
            u = [test_values(i)]
            f = godunov_flux_burgers(u, u)
            f_exact = burgers_physical_flux(u)
            call check(f(1) == f_exact(1))
        end do
    end subroutine test_godunov_consistency

    !>------------------------------------------------------------------------<!
    ! Hand-computed shock case: u_L=2, u_R=1. S_max=max(2,1)=2.
    ! F = 0.5*(f(2)+f(1)) - 0.5*2*(1-2) = 0.5*(2+0.5) + 1 = 2.25.
    !>------------------------------------------------------------------------<!
    subroutine test_rusanov_shock_hand_computed()
        real(rp) :: uL(1), uR(1), f(1)
        uL = [2.0_rp]
        uR = [1.0_rp]
        f = rusanov_flux_burgers(uL, uR)
        call check(f(1) == 2.25_rp)
    end subroutine test_rusanov_shock_hand_computed

    !>------------------------------------------------------------------------<!
    ! Same case, exact Godunov: u_L > u_R, shock speed s=(2+1)/2=1.5 >= 0,
    ! so F = f(u_L) = 2.0. NOTE: the one-step FV-update regression test in
    ! test_burgers.f90 (test_one_step_shock_hand_computed) depends on this
    ! exact value -- keep the two in sync if this case ever changes.
    !>------------------------------------------------------------------------<!
    subroutine test_godunov_shock_hand_computed()
        real(rp) :: uL(1), uR(1), f(1)
        uL = [2.0_rp]
        uR = [1.0_rp]
        f = godunov_flux_burgers(uL, uR)
        call check(f(1) == 2.0_rp)
    end subroutine test_godunov_shock_hand_computed

    !>------------------------------------------------------------------------<!
    ! u_L=0.5, u_R=1.5: rarefaction, entirely right-moving (u_L >= 0).
    ! F = f(u_L) = 0.125.
    !>------------------------------------------------------------------------<!
    subroutine test_godunov_right_moving()
        real(rp) :: uL(1), uR(1), f(1)
        uL = [0.5_rp]
        uR = [1.5_rp]
        f = godunov_flux_burgers(uL, uR)
        call check(f(1) == 0.125_rp)
    end subroutine test_godunov_right_moving

    !>------------------------------------------------------------------------<!
    ! Mirror image: u_L=-1.5, u_R=-0.5, entirely left-moving (u_R <= 0).
    ! F = f(u_R) = 0.125. Worth having both directions explicitly -- a
    ! hand-written branch structure can easily get one right and the
    ! other wrong.
    !>------------------------------------------------------------------------<!
    subroutine test_godunov_left_moving()
        real(rp) :: uL(1), uR(1), f(1)
        uL = [-1.5_rp]
        uR = [-0.5_rp]
        f = godunov_flux_burgers(uL, uR)
        call check(f(1) == 0.125_rp)
    end subroutine test_godunov_left_moving

    !>------------------------------------------------------------------------<!
    ! THE test most likely to catch a missing/wrong branch: u_L=-1 < 0 <
    ! u_R=1. The fan straddles the interface, so F = f(0) = 0 -- NOT
    ! f(u_L) and NOT f(u_R), which is exactly the mistake an incomplete
    ! branch structure tends to make. This is Phase 2's
    ! test_transonic_rarefaction, expressed as a pure flux check.
    !>------------------------------------------------------------------------<!
    subroutine test_godunov_transonic()
        real(rp) :: uL(1), uR(1), f(1)
        uL = [-1.0_rp]
        uR = [1.0_rp]
        f = godunov_flux_burgers(uL, uR)
        call check(f(1) == 0.0_rp)
    end subroutine test_godunov_transonic

    !>------------------------------------------------------------------------<!
    ! Rusanov is NOT required to match the exact flux -- it's a different,
    ! more-diffusive approximation, not an approximation OF the exact
    ! Godunov flux specifically. For the same transonic case, Rusanov
    ! gives -0.5, not 0. This test documents that divergence explicitly
    ! so a future reader doesn't mistake it for a bug in either function.
    !>------------------------------------------------------------------------<!
    subroutine test_rusanov_transonic_sanity()
        real(rp) :: uL(1), uR(1), f(1)
        uL = [-1.0_rp]
        uR = [1.0_rp]
        f = rusanov_flux_burgers(uL, uR)
        call check(f(1) == -0.5_rp)
    end subroutine test_rusanov_transonic_sanity

end module test_unit_burgers_flux
