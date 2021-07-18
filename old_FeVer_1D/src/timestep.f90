SUBROUTINE TIMESTEP
    USE GLOBALS
    IMPLICIT NONE
    REAL(KIND=8) :: dist,dt1,dt2,diffusivity
    dt1 = HUGE(1.d0)

    DO i=1,nr
    	dist = ABS(x(i+1) - x(i))
        dt1  = MIN(dist/ABS(u(i)),dt1)
    ENDDO

    dt2 = HUGE(1.d0)
    IF ( difs > 0.d0 ) THEN
        DO i = 1,nr
            dist = ABS(x(i+1) - x(i))
            dist = 0.5d0*(dist**2)/diffusivity(i-1+4,i+4)
            dt2 = min(dist,dt2)
        ENDDO
        IF ( .not. useImplicitSolve ) THEN
                dt1 = min(dt1,dt2)
        ENDIF
    ENDIF
    !dt = dt1
    dt = cfl * dt1
    dt = max(dtmin,min(dtmax,dt,tmax-t))
    RETURN
END SUBROUTINE TIMESTEP
