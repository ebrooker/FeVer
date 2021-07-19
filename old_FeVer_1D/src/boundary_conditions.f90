SUBROUTINE BOUNDARY_CONDITIONS ( var )
    USE GLOBALS, ONLY: i,ng,nx,nl,nr
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(nx+2*ng), INTENT(INOUT) :: var
    !! FILL IN GUARD CELLS, LHS, THEN, RHS
    DO i = 1,nl-1
        var(i) = var(nr-ng+i)
    ENDDO
    DO i = nr+1,2*ng+nx
        var(i) = var(i+ng-nr)
    ENDDO
    RETURN
END SUBROUTINE BOUNDARY_CONDITIONS