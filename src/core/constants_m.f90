module constants_m
    use kinds_m, only : rp
    implicit none

    !!> A small value
    real(rp), parameter :: small = 1e-10_rp

    !!> Boltzmann constant
    real(rp), parameter :: k_B = 1.38064852e-16_rp

    !!> Hydrogen mass
    real(rp), parameter :: m_H = 1.6737236e-24_rp

    !!> Numerical value of PI
    real(rp), parameter :: pi = 4.0_rp * atan(1.0_rp)

    !!> Twice PI
    real(rp), parameter :: pi2 = 8.0_rp * atan(1.0_rp)

end module constants_m