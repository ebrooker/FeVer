SUBROUTINE ADVECTION_FLUXES ( a, al, ar, fluxvar )
    USE GLOBALS, ONLY: i,nx,ng,nl,nr,u
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN)  :: al(nx+2*ng), ar(nx+2*ng), a(nx+2*ng)
    REAL(KIND=8), INTENT(OUT) :: fluxvar(nx+2*ng)

    !!ADVECT
    DO i=nl,nr+1
        IF(u(i) > 0.d0) THEN
            fluxvar(i) = u(i)*al(i)
        ELSEIF(u(i) < 0.d0) THEN
            fluxvar(i) = u(i)*ar(i)
        ELSE
            fluxvar(i) = 0.5d0*(ar(i) + al(i))*(u(i-1) + u(i))
        ENDIF

    ENDDO
    RETURN
END SUBROUTINE ADVECTION_FLUXES

!! NEED TO ADD area TO FLUX CALCULATIONS, ASSUMING area OF 1.d0 AT PRESENT
