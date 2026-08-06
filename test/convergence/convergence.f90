!>------------------------------------------------------------------------<!
!> Test application for convergence tests. Acts as the test driver.       <!
!>------------------------------------------------------------------------<!
program testapp_convergence
    use fortuno_serial, only: execute_serial_cmd_app, test_list
    use test_convergence_advection, only: advection_tests => tests
    implicit none

    call execute_serial_cmd_app( &
        test_list([advection_tests()]) &
    )

end program testapp_convergence