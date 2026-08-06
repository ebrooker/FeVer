!>------------------------------------------------------------------------<!
!> Test application for integration tests. Acts as the test driver.       <!
!>------------------------------------------------------------------------<!
program testapp_integration
    use fortuno_serial, only: execute_serial_cmd_app, test_list
    use test_integration_advection, only: advection_tests => tests
    implicit none

    call execute_serial_cmd_app( &
        test_list([advection_tests()]) &
    )

end program testapp_integration