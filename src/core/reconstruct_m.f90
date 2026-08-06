module reconstruct_m
    use kinds_m, only : rp, ip
    implicit none
    private
    public :: reconstruct_constant, reconstruct_linear_minmod

contains

    subroutine reconstruct_constant(u, u_left, u_right)
        !! Simple piecewise constant flux reconstruction
        !!
        !! The assumption is that the whole cell is "filled" with the same data
        !! So the left and right fluxes for a given cell "i" would be equal to
        !! the cell centered average.
        !! u_left(:,i) == u(:,i) == u_right(:,i) 
        !!
        real(rp), intent(in) :: u(:,:)
        real(rp), intent(out) :: u_left(:,:), u_right(:,:)
        u_left(:,:) = u(:,:)
        u_right(:,:) = u(:,:)
    end subroutine reconstruct_constant

    subroutine reconstruct_linear_minmod(u, u_left, u_right)
        real(rp), intent(in) :: u(:,:)
        real(rp), intent(out) :: u_left(:,:), u_right(:,:)

    end subroutine reconstruct_linear_minmod

    pure elemental function minmod(a,b) result(s)
        real(rp), intent(in) :: a,b
        real(rp) :: s
    end function minmod

end module reconstruct_m