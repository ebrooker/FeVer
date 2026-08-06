!>------------------------------------------------------------------------------<!
!> Convergence tests: exercise grid_t + state_t + time_integration_m together,  <!
!> via advance_euler_explicit / compute_rhs / compute_dt. These are the tests   <!
!> that would catch a bug in how the pieces are wired together, even if every   <!
!> unit and integration test passes.                                            <!
!>------------------------------------------------------------------------------<!
module test_convergence_advection
    use kinds_m, only: rp, ip
    use constants_m, only : pi2
    use boundary_conditions_m, only : apply_periodic_bc
    use grid_m, only: grid_t
    use state_m, only: state_t
    use time_integration_m, only: compute_dt, advance_euler_explicit
    use fortuno_serial, only: test => serial_case_item, &
                              check => serial_check, test_list
    implicit none
    private
    public :: tests

contains

    function log2(x)
        real(rp), intent(in) :: x
        real(rp) :: log2
        log2 = log(x) / log(2.0_rp)
    end function log2

    !>------------------------------------------------------------------------<!
    !> Collects the grid tests as a returnable test_list                      <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            test("convergence_rate_piecewise_constant", test_convergence_rate_piecewise_constant) &
        ])
    end function tests


    !>------------------------------------------------------------------------<!
    !> Convergence-rate test: run the smooth-IC translation problem at        <!
    !> several resolutions, compute the L1 (or L2) error against the exact    <!
    !> solution at each, and confirm the observed order of accuracy is        <!
    !> close to 1 for piecewise-constant reconstruction (log-log slope of     <!
    !> error vs dx).                                                          <!
    !>                                                                        <!
    !> NOTE: this test is written now against reconstruct_constant only.      <!
    !> Once reconstruct_linear_minmod exists, duplicate this test (or         <!
    !> parametrize it) to confirm ~2nd order for the limited-linear case --   <!
    !> this is the single most direct piece of evidence that the limiter      <!
    !> is implemented correctly, more informative than eyeballing a plot.     <!
    !>------------------------------------------------------------------------<!
    subroutine test_convergence_rate_piecewise_constant()
        integer(ip), parameter :: n_resolutions = 5
        integer(ip) :: n_cells_list(n_resolutions)
        real(rp) :: errors(n_resolutions)
        real(rp) :: observed_order, top, bot
        integer(ip) :: r

        real(rp), parameter :: L=1.0_rp, a=1.0_rp, cfl=0.5_rp
        integer(ip), parameter :: n_ghost=1

        n_cells_list = [32, 64, 128, 256, 512]

        do r = 1, n_resolutions
            ! run the exact_translation_one_period problem at
            ! n_cells_list(r), compute L1 error = sum(abs(u_numerical -
            ! u_exact)) * dx (or L2 = sqrt(sum((u_num-u_exact)**2)*dx)),
            ! store into errors(r).
            block
                type(grid_t) :: g
                type(state_t) :: s

                real(rp) :: t_final, t, dt
                real(rp), allocatable :: u0(:)
                integer(ip) :: n_cells

                n_cells = n_cells_list(r)
                t_final = L / a
                t = 0.0_rp

                call g%initialize(n_cells, 0.0_rp, L, n_ghost)
                call s%initialize(1, g)
                s%u(1,:) = sin(pi2 * g%xc / L)
                u0 = s%u(1,1:g%n_cells)

                do while (t < t_final)
                    dt = compute_dt(g%dx, a, cfl)
                    dt = min(dt, t_final - t)
                    call apply_periodic_bc(g, s)
                    call advance_euler_explicit(g, s, dt, a)
                    t = t + dt
                end do

                errors(r) = sqrt(sum((s%u(1,1:g%n_cells) - u0)**2)*g%dx)

            end block
        end do

        ! Observed order from consecutive resolutions (assuming each
        ! successive n_cells doubles, as here): order = log2(errors(r) /
        ! errors(r+1)). Average over the pairs, or just check each pair
        ! individually is within a reasonable band of 1.0.
        !
        ! compute observsient effects, tight enough to catch a
        ! scheme that's actually 0th- or 2nd-order due to a bug.
        do r = 1,n_resolutions-2
            observed_order = log2(errors(r) / errors(r+1))
            call check(observed_order > 0.85_rp .and. observed_order < 1.15_rp)
        end do
    end subroutine test_convergence_rate_piecewise_constant

end module test_convergence_advection