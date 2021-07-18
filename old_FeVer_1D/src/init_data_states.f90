SUBROUTINE INIT_DATA_STATES
	USE GLOBALS
	USE INIT_SIMULATION_INTERFACE, ONLY: INIT_DATA_FROM_FILE
	INTEGER(KIND=4) :: stat

    PRINT *, "[STATES]: Initializing state vectors"

	IF ( readflag ) THEN
        PRINT *, "----[DENSITY]: Reading in density data from ", TRIM(rhofn)
		CALL INIT_DATA_FROM_FILE( rhofn, c, stat)
		c0 = c !! Save initial state for "density", really just unnamed quantity, need to fix this

		!! Add more quantities here as needed

	ELSE
		SELECT CASE (TRIM(icflag))

            CASE ("sine")
                ! SINE WAVE
                PRINT *, "----[DENSITY]: Using ", TRIM(icflag), " function"
                c = SIN(2.0d0*PI*x)

            CASE ("square_pulse")
                ! SQUARE PULSE WAVE
                PRINT *, "----[DENSITY]: Using ", TRIM(icflag), " function"
                DO i = nl,nr
                    IF (x(i) .gt. 0.4*xmax .and. x(i) .lt. 0.6*xmax) THEN
                        c(i) = 2.d0
                    ELSE
                        c(i) = 1.d0
                    ENDIF
                ENDDO


            CASE ("square_pulse_adr")
                ! SQUARE PULSE WAVE FOR ADR STUDY
                PRINT *, "----[DENSITY]: Using square pulse wave for adr study"
                DO i = nl,nr
                        IF (x(i) .gt. 1.6d0 .and. x(i) .lt. 2.2d0) THEN
                                c(i) = 2.d0
                        ELSE
                                c(i) = 1.d0
                        ENDIF
                ENDDO

        END SELECT
    ENDIF
    RETURN
END SUBROUTINE INIT_DATA_STATES

SUBROUTINE EXACT ( var ) 
    USE GLOBALS
    IMPLICIT NONE
    REAL(KIND=8), INTENT(OUT) :: var(2*ng+nx)
    SELECT CASE (TRIM(icflag))

        CASE ("sine")
            ! SINE WAVE
            var = SIN(2.0d0*PI*x + t)

        CASE ("square_pulse")
            ! SQUARE PULSE WAVE
            DO i = nl,nr
                IF (x(i) >= 0.3*xmax .and. x(i) <= 0.5*xmax) THEN
                    var(i) = 2.d0
                ELSE
                    var(i) = 1.d0
                ENDIF
            ENDDO
    END SELECT
    RETURN
END SUBROUTINE EXACT
