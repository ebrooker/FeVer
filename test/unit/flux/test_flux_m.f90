module test_unit_flux_m
    use test_unit_burgers_flux, only : tests_burgers_flux => tests
    use fortuno_serial, only: test_list
    private
    public :: tests

contains

    function tests()
        type(test_list) :: tests
        tests = test_list([ &
            tests_burgers_flux() &
        ])
    end function tests
end module test_unit_flux_m