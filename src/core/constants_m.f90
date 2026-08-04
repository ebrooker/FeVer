module constants_m
    use kinds_m, only : rp
    implicit none

    real(rp), parameter :: k_B = 1.38064852e-16_rp
    real(rp), parameter :: m_H = 1.6737236e-24_rp
    real(rp), parameter :: pi = 4.0_rp * atan(1.0_rp)
    real(rp), parameter :: pi2 = 8.0_rp * atan(1.0_rp)

end module constants_m