module burgers_flux_m
    use kinds_m, only : rp
    implicit none
    private
    public :: rusanov_flux_burgers, godunov_flux_burgers, burgers_physical_flux

contains

    pure function burgers_physical_flux(u) result(F)
        real(rp), intent(in) :: u(:)
        real(rp) :: F(size(u))
        F = 0.5_rp * u * u
    end function burgers_physical_flux

    pure function godunov_flux_burgers(uL, uR) result(F)
        real(rp), intent(in) :: uL(:), uR(:)
        real(rp) :: F(size(uL)), S, L, R, U(size(uL))
        integer :: n

        do n = 1, size(F)
            L = uL(n)
            R = uR(n)
            S = 0.5_rp * (L + R)

            if (L > R) then
                !!> Shock case
                if (S > 0.0_rp) then
                    U(n) = L
                else if (S < 0.0_rp) then
                    U(n) = R
                end if
            else
                !!> Rarefaction case
                if (L > 0.0_rp) then
                    U(n) = L
                else if (R < 0.0_rp) then
                    U(n) = R
                else
                    U = 0.0_rp
                end if
            end if
        end do
        F = burgers_physical_flux(U)

    end function godunov_flux_burgers

    pure function rusanov_flux_burgers(uL, uR) result(F)
        real(rp), intent(in) :: uL(:), uR(:)
        real(rp) :: F(size(uL)), S_max(size(uL))
        S_max = max(abs(uL), abs(uR))
        F = 0.5_rp * (burgers_physical_flux(uL) + burgers_physical_flux(uR)) - 0.5_rp * S_max * (uR - uL)
    end function rusanov_flux_burgers

end module burgers_flux_m