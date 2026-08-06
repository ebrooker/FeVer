module flux_m
    use kinds_m, only : rp
    implicit none
    private
    public :: upwind_flux_advection

contains

    pure function upwind_flux_advection(uL, uR, a) result(F)
        real(rp), intent(in) :: uL(:), uR(:), a
        real(rp) :: F(size(uL))
        if (a > 0) then
            F = a*uL
        else
            F = a*uR
        end if
    end function upwind_flux_advection

end module flux_m