SUBROUTINE DIFFUSION_FLUXES ( a, fluxvar )
    USE GLOBALS, ONLY: i,nx,ng,nl,nr,dx,x,difs
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN)  :: a(nx+2*ng)
    REAL(KIND=8), INTENT(OUT) :: fluxvar(nx+2*ng)
    REAL(KIND=8)              :: diffusivity

    !!DIFFUSE
    DO i=nl,nr+1
        fluxvar(i) = fluxvar(i) - diffusivity(i-1,i) * (a(i) - a(i-1)) / dx(i)
    ENDDO
    RETURN
END SUBROUTINE DIFFUSION_FLUXES

!! NEED TO ADD area TO FLUX CALCULATIONS, ASSUMING area OF 1.d0 AT PRESENT


REAL(KIND=8) FUNCTION diffusivity( il, ir )
    USE GLOBALS
    IMPLICIT NONE
    INTEGER(KIND=4), INTENT(IN) :: il,ir
    REAL(KIND=8)                :: xmid
    IF ( il == ir) THEN
        xmid = x(il)
    ELSE
        xmid = x(il) + 0.5d0 * (x(ir)-x(il))
    ENDIF
    diffusivity = difs * 0.5d0 * (TANH(3.d0*(xmid-2.2d0)) + 1.d0)
END FUNCTION diffusivity