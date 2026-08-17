!>------------------------------------------------------------------------<!
!> Unit tests for reconstruct_m, exercised with no grid, no timesteps     <!
!> no flux -- just the reconstruction routines against small, hand-built  <!
!> arrays where the expected output can be worked out on paper            <!
!>------------------------------------------------------------------------<!
module test_unit_reconstruct
    use kinds_m, only: rp, ip
    use fortuno_serial, only: is_equal, test => serial_case_item, &
                              check => serial_check, test_list
    use reconstruct_m, only: reconstruct_constant, reconstruct_linear_minmod, reconstruct_linear
    implicit none
    private
    public :: tests

contains

    !>------------------------------------------------------------------------<!
    !> Collects the reconstruction tests as a returnable test_list            <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            test("reconstruct_constant_matches_input", test_reconstruct_constant_matches_input), &
            test("reconstruct_constant_left_equals_right", test_reconstruct_constant_left_equals_right), &
            test("reconstruct_constant_preserves_shape", test_reconstruct_constant_preserves_shape), &
            test("reconstruct_constant_multivar", test_reconstruct_constant_multivar) &
        ])
    end function tests

    !>------------------------------------------------------------------------<!
    !> u_left and u_right should both equal u exactly, cell by cell --        <!
    !> piecewise-constant reconstruction has zero slope by definition.        <!
    !>------------------------------------------------------------------------<!
    subroutine test_reconstruct_constant_matches_input()
        real(rp), allocatable :: u(:,:), u_left(:,:), u_right(:,:)

        allocate(u(1,5))
        allocate(u_left(1,5))
        allocate(u_right(1,5))

        u(1,:) = [1.0_rp, 2.0_rp, 3.0_rp, 4.0_rp, 5.0_rp]

        call reconstruct_constant(u, u_left, u_right)

        call check(all(u_left == u))
        call check(all(u_right == u))
    end subroutine test_reconstruct_constant_matches_input

    !>------------------------------------------------------------------------<!
    !> Directly confirm u_left(i) == u_right(i) for every i -- this is the    <!
    !> specific identity that distinguishes piecewise-constant from any       <!
    !> reconstruction with nonzero slope                                      <!
    !>------------------------------------------------------------------------<!
    subroutine test_reconstruct_constant_left_equals_right()
        real(rp), allocatable :: u(:,:), u_left(:,:), u_right(:,:)

        allocate(u(1,4))
        allocate(u_left(1,4))
        allocate(u_right(1,4))

        u(1,:) = [-1.0_rp, 0.5_rp, 2.5_rp, 100.0_rp]

        call reconstruct_constant(u, u_left, u_right)

        call check(all(u_left == u_right))
    end subroutine test_reconstruct_constant_left_equals_right

    !>------------------------------------------------------------------------<!
    !> Confirm the routine doesn't silently change array shape/bounds --      <!
    !> cheap to check now, and a real failure mode if reconstruct_constant    <!
    !> is ever called with mismatched u_left/u_right allocations.             <!
    !>------------------------------------------------------------------------<!
    subroutine test_reconstruct_constant_preserves_shape()
        real(rp), allocatable :: u(:,:), u_left(:,:), u_right(:,:)

        allocate(u(1,6))
        allocate(u_left(1,6))
        allocate(u_right(1,6))

        u(1,:) = 1.0_rp

        call reconstruct_constant(u, u_left, u_right)

        call check(size(u_left,dim=1) == size(u,dim=1))
        call check(size(u_left,dim=2) == size(u,dim=2))
        call check(size(u_right,dim=1) == size(u,dim=1))
        call check(size(u_right,dim=2) == size(u,dim=2))
    end subroutine test_reconstruct_constant_preserves_shape

    !>------------------------------------------------------------------------<!
    !> Confirm reconstruction is applied independently per row                <!
    !> Use n_vars=2 here with clearly distinguishable per-row values so a bug <!
    !> that mixes rows together would be caught.                              <!
    !>------------------------------------------------------------------------<!
    subroutine test_reconstruct_constant_multivar()
        real(rp), allocatable :: u(:,:), u_left(:,:), u_right(:,:)

        allocate(u(2,3))
        allocate(u_left(2,3))
        allocate(u_right(2,3))

        u(1,:) = [1.0_rp, 2.0_rp, 3.0_rp]
        u(2,:) = [10.0_rp, 20.0_rp, 30.0_rp]

        call reconstruct_constant(u, u_left, u_right)

        call check(all(u_left(1,:) == u(1,:)))
        call check(all(u_left(2,:) == u(2,:)))
        call check(all(u_right(1,:) == u(1,:)))
        call check(all(u_right(2,:) == u(2,:)))
    end subroutine test_reconstruct_constant_multivar

    !>------------------------------------------------------------------------<!
    ! TODO once reconstruct_linear_minmod exists:
    !  - test_minmod_zero_on_local_extremum: construct u where cell i is a
    !    local max/min relative to its neighbors; confirm the limited slope
    !    is exactly zero there (this is the defining TVD property of minmod).
    !  - test_minmod_matches_centered_on_linear_data: construct u as a
    !    perfectly linear ramp (u(i) = c0 + c1*i); confirm the limited slope
    !    equals the unlimited centered-difference slope exactly, since a
    !    limiter should never clip a genuinely smooth linear profile.
    !  - test_minmod_reduces_to_constant_reconstruction_at_discontinuity:
    !    construct a single-cell spike (or a step) where the two one-sided
    !    differences have opposite sign; confirm the limited slope is zero,
    !    i.e. reconstruct_linear_minmod reduces to reconstruct_constant's
    !    behavior exactly at that cell.
    !  - test_edge_values_consistent_with_slope: for a known slope sigma_i,
    !    confirm u_right(i) = u(i) + (dx/2)*sigma_i and
    !    u_left(i) = u(i) - (dx/2)*sigma_i exactly.
    !>------------------------------------------------------------------------<!

end module test_unit_reconstruct