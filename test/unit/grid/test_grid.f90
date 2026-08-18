!>------------------------------------------------------------------------<!
!> Unit test module for grid_m to excercise the grid_t object             <!
!>------------------------------------------------------------------------<!
module test_unit_grid
    use fortuno_serial, only : test => serial_case_item, check => serial_check, test_list, is_close, is_equal
    use kinds_m
    use grid_m, only : grid_t
    implicit none
    private
    public :: tests


contains

    !>------------------------------------------------------------------------<!
    !> Collects the grid tests as a returnable test_list                      <!
    !>------------------------------------------------------------------------<!
    function tests()
        type(test_list) :: tests
        tests = test_list([test("test_init_grid", test_init_grid)])
    end function tests


    !>------------------------------------------------------------------------<!
    !> Exercises initializing the grid object                                 <!
    !>------------------------------------------------------------------------<!
    subroutine test_init_grid()
        type(grid_t) :: grid
        call grid%initialize(10, 0.0_rp, 1.0_rp, 2)
        call check(is_equal(grid%n_cells, 10_ip))
        call check(is_equal(grid%n_ghost, 2_ip))

        call check(is_equal(grid%ilo, -1_ip))
        call check(is_equal(grid%ihi, 12_ip))

        !! Check grid spacing value
        call check(grid%dx == 0.1_rp)

        !! Check interface start and end node are equal to x_min and x_max
        call check(grid%xi(0) == 0.0_rp)
        call check(grid%xi(grid%n_cells) == 1.0_rp)

        !! Check that the first and last cell center are dx/2 in from x_min
        !! and x_max, respectively
        call check(grid%xc(1) == 0.05_rp)
        call check(grid%xc(grid%n_cells) == 0.95_rp)
    end subroutine test_init_grid

end module test_unit_grid