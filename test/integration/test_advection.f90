!>------------------------------------------------------------------------------<!
!> Integration tests: exercise grid_t + state_t + time_integration_m together,  <!
!> via advance_euler_explicit / compute_rhs. These are the tests that would     <!
!> catch a bug in how the pieces are wired together, even if every unit test    <!
!> passes                                                                       <!
!>------------------------------------------------------------------------------<!
module test_integration_advection
    use kinds_m, only: rp, ip
    use constants_m, only : pi2, epsilon
    use fortuno_serial, only: test => serial_case_item, &
                              check => serial_check, test_list

    use boundary_conditions_m, only : apply_periodic_bc
    use grid_m, only: grid_t
    use state_m, only: state_t
    use time_integration_m, only: compute_dt, advance_euler_explicit
    implicit none
    private
    public :: tests

 contains

    !>------------------------------------------------------------------------<!
    !> Collects the grid tests as a returnable test_list                      <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            test("single_step_matches_hand_computation", test_single_step_hand_computed), &
            test("exact_translation_one_period", test_exact_translation_one_period), &
            test("constant_state_is_a_fixed_point", test_constant_state_fixed_point) &
        ])
    end function tests

    !>------------------------------------------------------------------------<!
    !> Integration test: a single timestep on a tiny, hand-picked periodic    <!
    !> state, with the post-step values worked out by hand and checked        <!
    !> directly. This is the cheapest possible test that ghost-cell fill +    <!
    !> reconstruction + flux + update are wired together correctly --         <!
    !> failing here points at plumbing, not at scheme accuracy.               <!
    !>                                                                        <!
    !> NOTE: assumes ghost cells are filled by the CALLER (per your current   <!
    !> advance_euler_explicit docstring, "ghost cells must be filled prior    <!
    !> to call"). Fill them explicitly and directly here rather than via a    <!
    !> separate boundary-condition routine, precisely so this test does not   <!
    !> depend on that routine also being correct -- keeps this a targeted     <!
    !> single-step check.                                                     <!
    !>------------------------------------------------------------------------<!
    subroutine test_single_step_hand_computed()
        type(grid_t) :: grid
        type(state_t) :: state
        real(rp) :: a, dt, expected(4)

        ! set expectations
        expected = [4.0_rp, 1.0_rp, 2.0_rp, 3.0_rp]

        ! construct grid with n_cells=4, n_ghost=1, dx known (e.g. 1.0)
        call grid%initialize(n_cells=4_ip, n_ghost=1_ip, x_min=0.0_rp, x_max=4.0_rp)

        ! construct state with n_vars=1, u = [1,2,3,4] on interior cells
        call state%initialize(n_vars=1_ip, grid=grid)
        state%u(1,1:4) = [1.0_rp, 2.0_rp, 3.0_rp, 4.0_rp]

        ! fill ghost cells by hand for periodic BC:
        !   u(:,0) = u(:,4)   (left ghost = rightmost interior cell)
        !   u(:,5) = u(:,1)   (right ghost = leftmost interior cell)
        state%u(:,0) = state%u(:,4)
        state%u(:,5) = state%u(:,1)

        ! positive direction advection
        a = 1.0

        ! dt chosen well within the CFL=1.0 limit CFL*dx / abs(a)
        dt = 1.0

        ! advance solution by one timestep
        call advance_euler_explicit(grid, state, dt, a)
        
        ! should 
        call check(all( &
                  state%u(1,1:4) >= expected - epsilon &
            .and. state%u(1,1:4) <= expected + epsilon &
        ))
    end subroutine test_single_step_hand_computed

    !>------------------------------------------------------------------------<!
    !> Exact-solution integration test: smooth periodic IC, advance for       <!
    !> exactly one full period (t = L/a), confirm the numerical result        <!
    !> matches the (translated-by-exactly-one-period, i.e. unchanged)         <!
    !> initial condition to within a tolerance appropriate for a diffusive    <!
    !> first-order scheme at the chosen resolution.                           <!
    !>------------------------------------------------------------------------<!
    subroutine test_exact_translation_one_period()
        type(grid_t) :: grid
        type(state_t) :: state
        real(rp), allocatable :: u0(:)
        real(rp) :: a, cfl, dt, t, t_final, L, error
        integer(ip) :: n_cells, i

        ! construct grid over [0, L) with periodic topology in mind,
        ! n_cells chosen for a moderate resolution (e.g. 64).
        n_cells = 64_ip
        L = 1.0_rp
        call grid%initialize(n_cells=n_cells, n_ghost=1_ip, x_min=0.0_rp, x_max=L)

        ! initialize u0(i) = sin(2*pi*x_i/L); store a copy for
        ! later comparison, and also load it into state%u's interior cells.
        if (allocated(u0)) deallocate(u0)
        allocate(u0(grid%ilo:grid%ihi))
        u0 = sin(pi2 * grid%xc / L)

        ! construct the state object and initialize u
        call state%initialize(n_vars=1_ip, grid=grid)
        state%u(1,:) = u0(:)

        ! a > 0, cfl = 0.5, t_final = L/a (exactly one period)
        a = 1.0
        cfl = 0.5_rp
        t_final = L / a
        ! time loop:
        do while (t < t_final)
            dt = compute_dt(grid%dx, a, cfl)
            dt = min(dt, t_final - t)
            call apply_periodic_bc(grid, state)
            call advance_euler_explicit(grid, state, dt, a)
            t = t + dt
        end do

        ! compare state%u(1, interior) against u0 with a tolerance
        !   loose enough for first-order diffusive smearing at this
        !   resolution (e.g. L1 error below some empirically-reasonable
        !   bound) -- NOT machine precision; some smearing is correct and
        !   expected for piecewise-constant reconstruction.
        error = sqrt(sum((state%u(1,1:n_cells) - u0(1:n_cells))**2))

        ! computed error for setup should be ~0.8087226
        call check((0.8087225_rp < error .and. error < 0.8087227_rp))

    end subroutine test_exact_translation_one_period

    !>------------------------------------------------------------------------<!
    !> Degenerate-case sanity check: a spatially uniform state should be      <!
    !> an exact fixed point of the scheme (flux is continuous everywhere,     <!
    !> so the divergence is exactly zero at every interface). Cheap to        <!
    !> check and a good early warning for an indexing bug that only shows     <!
    !> up as nonzero output on supposedly-uniform input.                      <!
    !>------------------------------------------------------------------------<!
    subroutine test_constant_state_fixed_point()
        type(grid_t) :: grid
        type(state_t) :: state
        real(rp) :: a, dt
        real(rp), allocatable :: u_before(:)

        call grid%initialize(n_cells=32_ip, n_ghost=2_ip, x_min=0.0_rp, x_max=32.0_rp)
        call state%initialize(n_vars=2, grid=grid)
        state%u(:,1:grid%n_cells) = 2.0_rp
        
        u_before = state%u(1,1:grid%n_cells)

        call apply_periodic_bc(grid, state)

        a = 1.0
        dt = 1.0 ! dx=1.0, a=1.0, cfl=1.0

        ! take one or several steps.
        call advance_euler_explicit(grid, state, dt, a)
        call advance_euler_explicit(grid, state, dt, a)
        call advance_euler_explicit(grid, state, dt, a)

        ! confirm state%u(:,interior) is unchanged (is_equal or a
        !   tight tolerance) -- any deviation here indicates a bug in the
        !   flux or update formula, not a scheme-accuracy issue.
        call check(all(state%u(1,1:grid%n_cells) == u_before))
        call check(all(state%u(2,1:grid%n_cells) == u_before))

    end subroutine test_constant_state_fixed_point

end module test_integration_advection