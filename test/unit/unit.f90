!>------------------------------------------------------------------------<!
!> Test application for unit tests. Acts as the test driver.              <!
!>------------------------------------------------------------------------<!
program testapp_unit
    use test_unit_boundary_conditions, only: bc_tests => tests
    use test_unit_flux, only: flux_tests => tests
    use test_unit_grid, only: grid_tests => tests
    use test_unit_initial_conditions, only: ic_tests => tests
    use test_unit_reconstruct, only: reconstruct_tests => tests
    use test_unit_state, only : state_tests => tests
    use test_unit_time_integration, only: time_integration_tests => tests
    use fortuno_serial, only: execute_serial_cmd_app, test_list, suite => serial_suite_item
    implicit none

    ! concatenate all test_list's into one, then:
    call execute_serial_cmd_app( &
        test_list([ &
            bc_tests(), &
            flux_tests(), &
            grid_tests(), &
            ic_tests(), &
            reconstruct_tests(), &
            state_tests(), &
            time_integration_tests() &
        ]) &
    )
end program testapp_unit