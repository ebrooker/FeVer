SUBROUTINE INIT_DATA_MESH
    USE GLOBALS
    USE INIT_SIMULATION_INTERFACE, ONLY: INIT_DATA_FROM_FILE
    IMPLICIT NONE
    INTEGER(KIND=4) :: stat, nn

    PRINT *, "[GRID]: Initializing simulation grid"

    IF ( readflag ) THEN
    	PRINT *, "[GRID]: Reading in grid data from ", TRIM(posfn)
    	CALL INIT_DATA_FROM_FILE( posfn, x, stat )
    	dx = x(2:nx+2*ng) - x(1:nx+2*ng-1)

    ELSE
	    SELECT CASE (TRIM(gridflag))	    

			CASE('ug')
				PRINT *, "[GRID]: Using uniform grid"
			    dx = (xmax-xmin)/dble(nx)
			    DO i = 1,2*ng+nx
			    	x(i) = (i-ng-0.5d0)*dx(i) + xmin
			    ENDDO

			CASE('non-ug')
				PRINT *, ""
				PRINT *, "non-uniform grid not implemented, yet"
				PRINT *, ""
				PRINT *, "...aborting program..."
				PRINT *, ""
				STOP
		END SELECT
	ENDIF
	RETURN
END SUBROUTINE INIT_DATA_MESH