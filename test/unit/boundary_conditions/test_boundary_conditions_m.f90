module test_unit_boundary_conditions_m
    use test_unit_outflow_bc, only : tests_outflow_bc => tests
    use test_unit_periodic_bc, only : tests_periodic_bc => tests
    use fortuno_serial, only: test_list
    private
    public :: tests

contains

    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            tests_outflow_bc(), &
            tests_periodic_bc() &
        ])
    end function tests
end module test_unit_boundary_conditions_m