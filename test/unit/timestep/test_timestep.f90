!>------------------------------------------------------------------------<!
!> Unit test module for time_integration_m to excercise the grid_t object <!
!>------------------------------------------------------------------------<!
module test_unit_timestep
    use kinds_m, only: rp, ip
    use fortuno_serial, only: test => serial_case_item, &
                              check => serial_check, test_list
    use timestep_m, only: compute_dt, compute_dt_burgers
    use state_m, only : state_t
    use grid_m, only : grid_t
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
            test("advection_dt_matches_cfl_formula", test_advection_dt_matches_cfl_formula), &
            test("burgers_dt_domain_max", test_burgers_dt_domain_max) &
        ])
    end function tests

    !>------------------------------------------------------------------------<!
    !> Test compute_dt to confirm the formula directly                        <!
    !>------------------------------------------------------------------------<!
    subroutine test_advection_dt_matches_cfl_formula()
        real(rp) :: dx, a, cfl, dt
        dx = 0.1_rp
        a = 2.0_rp
        cfl = 0.5_rp

        dt = compute_dt(dx, a, cfl)

        call check(dt == cfl * dx / abs(a))
    end subroutine test_advection_dt_matches_cfl_formula


    ! --------------------------------------------------------------------
    ! compute_dt_burgers must use the domain-wide MAX of |u|, recomputed
    ! every call -- unlike Phase 1's compute_dt, which could use a fixed
    ! constant a. Deliberately put the max away from both array ends to
    ! catch an off-by-one or endpoint-only bug.
    ! --------------------------------------------------------------------
    subroutine test_burgers_dt_domain_max()
        type(grid_t) :: g
        type(state_t) :: s
        integer, parameter :: n_cells = 5, n_ghost = 2, n_vars = 1
        real(rp), parameter :: x0 = 0.0_rp, L = 0.5_rp
        real(rp) :: cfl, dt
        cfl = 0.5_rp

        call g%initialize(n_cells, x0, L, n_ghost)
        call s%initialize(n_vars, g)
        s%u(1,:) = [0.5_rp, 1.0_rp, -3.0_rp, 1.0_rp, 0.5_rp]   ! max|u| = 3, at cell 3
        dt = compute_dt_burgers(g, s, cfl)

        call check(dt == cfl * g%dx / 3.0_rp)
    end subroutine test_burgers_dt_domain_max


end module test_unit_timestep