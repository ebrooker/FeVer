SUBROUTINE SOURCE_TERMS ( time, dtstep )
    USE GLOBALS
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: time,dtstep
    REAL(KIND=8)             :: tempX

    SELECT CASE (TRIM(sourceflag))

    	CASE("delta")
			! Delta pulse source term
			!PRINT *, "[SOURCES]: Using ", TRIM(sourceflag), " source"

			tempX = (xmax-xmin)*sourceX + xmin

			DO i = nl,nr
				sources(i) = 0.d0

				IF ( x(i)-0.5d0*dx(i) <= tempX .and. x(i)+0.5d0*dx(i) > tempX ) THEN
					IF ( time >= sourceTi ) THEN
						IF ( time + dtstep <= sourceTf ) THEN
							sources(i) = sourceA
						ENDIF
					ENDIF
				ENDIF

				c(i) = c(i) + sources(i)*dt
			ENDDO

	END SELECT
	RETURN
END SUBROUTINE SOURCE_TERMS
