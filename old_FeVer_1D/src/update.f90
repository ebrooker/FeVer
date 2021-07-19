SUBROUTINE UPDATE ( fl, st )
    USE GLOBALS, ONLY:  nl, nr, ng, nx, i, dt, dx
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN)    :: fl(2*ng+nx)
    REAL(KIND=8), INTENT(INOUT) :: st(2*ng+nx)
    REAL(KIND=8)                :: dtdx,ddx,adv,dfs,vdx
    DO i = nl,nr
        dtdx = dt/dx(i)
        ddx = st(i)
        st(i) = st(i) + dtdx*(fl(i) - fl(i+1))
    ENDDO
    RETURN
END SUBROUTINE UPDATE
