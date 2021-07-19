SUBROUTINE INIT_DATA_VELOCITY
	USE GLOBALS
	USE INIT_SIMULATION_INTERFACE, ONLY: INIT_DATA_FROM_FILE
	IMPLICIT NONE
	INTEGER(KIND=4) :: stat

	PRINT *, "[VELOCITY]: Initializing velocity field"
	IF ( readflag ) THEN
		PRINT *, "[VELOCITY]: Reading in velocity data from ", TRIM(velfn)
		CALL INIT_DATA_FROM_FILE( velfn, u, stat )

	ELSE
		
		SELECT CASE (TRIM(velflag))

			CASE("unpos")
				PRINT *, "[VELOCITY]: Using uniform positive velocity field"
				u = 1.0d0*v

			CASE("unneg")
				PRINT *, "[VELOCITY]: Using uniform negative velocity field"
				u = -1.0d0*v

			CASE("expos")
				PRINT *, "[VELOCITY]: Using exponential positive velocity field"
				u = 1.0d0*DEXP(x)*v

			CASE("exneg")
				PRINT *, "[VELOCITY]: Using exponential negative velocity field"
				u = -1.0d0*DEXP(x)*v
		END SELECT

	ENDIF
	RETURN
END SUBROUTINE INIT_DATA_VELOCITY