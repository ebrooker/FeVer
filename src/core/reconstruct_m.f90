module reconstruct_m
    use kinds_m, only : rp, ip
    implicit none
    private
    public :: reconstruct_constant, reconstruct_linear_minmod, reconstruct_linear

contains

    subroutine reconstruct_constant(u, u_left, u_right)
        !! Simple piecewise constant flux reconstruction
        !!
        !! The assumption is that the whole cell is "filled" with the same data
        !! So the left and right fluxes for a given cell "i" would be equal to
        !! the cell centered average.
        !! u_left(:,i) == u(:,i) == u_right(:,i) 
        !!
        real(rp), intent(in), allocatable :: u(:,:)
        real(rp), intent(inout), allocatable :: u_left(:,:), u_right(:,:)
        u_left(:,:) = u(:,:)
        u_right(:,:) = u(:,:)
    end subroutine reconstruct_constant


    subroutine reconstruct_linear(u, u_left, u_right, ilo, ihi)
        real(rp), intent(in), allocatable :: u(:,:)
        real(rp), intent(inout), allocatable :: u_left(:,:), u_right(:,:)
        integer, intent(in) :: ilo, ihi

        integer :: i, n
        real(rp) :: slope(ilo-1:ihi+1)

        do n = 1, ubound(u, dim=1)

            do i = ilo-1, ihi+1
                slope(i) = 0.5_rp * (u(n,i+1) - u(n,i-1))
            end do

            do i = ilo-1, ihi+1
                u_left(n,i) = u(n,i) - slope(i)
                u_right(n,i) = u(n,i) + slope(i)
            end do

        end do

    end subroutine reconstruct_linear

    subroutine reconstruct_linear_minmod(u, u_left, u_right, ilo, ihi)
        real(rp), intent(in), allocatable :: u(:,:)
        real(rp), intent(inout), allocatable :: u_left(:,:), u_right(:,:)
        integer, intent(in) :: ilo, ihi
        integer :: i, n
        real(rp) :: slope(ilo-1:ihi+1)

        do n = 1, ubound(u, dim=1)
            do i = ilo-1, ihi+1
                slope(i) = minmod( (u(n,i) - u(n,i-1)), (u(n,i+1) - u(n,i)) )
            end do

            do i = ilo-1, ihi+1
                u_left(n,i) = u(n,i) + 0.5_rp * slope(i)
                u_right(n,i) = u(n,i) - 0.5_rp * slope(i)
            end do
        end do

    end subroutine reconstruct_linear_minmod

    pure elemental function minmod(a,b) result(s)
        real(rp), intent(in) :: a,b
        real(rp) :: s
        if (a*b <= 0.0_rp) then
            s = 0.0_rp
        else if (abs(a) < abs(b)) then
            s = a
        else
            s = b
        end if
    end function minmod

    pure elemental function maxmod(a,b) result(s)
        real(rp), intent(in) :: a,b
        real(rp) :: s
        if (a*b <= 0.0_rp) then
            s = 0.0_rp
        else if (abs(a) > abs(b)) then
            s = a
        else
            s = b
        end if
    end function maxmod


end module reconstruct_m