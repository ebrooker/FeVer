module functions_1d_m
    use constants_m, only : pi2
    use kinds_m, only : rp

contains

    pure elemental function sine(x) result(u)
        real(rp), intent(in) :: x
        real(rp) :: u
        u = sin(pi2*x)
    end function sine

    pure elemental function square_pulse(x, xlo, xhi) result(u)
        real(rp), intent(in) :: x, xlo, xhi
        real(rp) :: u
        u = 0.0_rp
        if (xlo < x .and. x < xhi) u = 1.0_rp
    end function square_pulse

end module functions_1d_m