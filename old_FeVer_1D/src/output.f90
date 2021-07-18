SUBROUTINE OUTPUT_STATE
	USE GLOBALS
	IMPLICIT NONE
	CHARACTER(LEN=MAXCHARS) :: tmp
	CALL WRITE_FILENAME( "../data/data" )
	WRITE(tmp,'(A,"_",I0.5)') TRIM(filename), step
	OPEN(UNIT=22,FILE=tmp,STATUS="UNKNOWN")
	DO j = ng+1,nx+ng
		WRITE(22,*) x(j), c(j), c0(j)
	ENDDO
	CLOSE(UNIT=22)
	RETURN
END SUBROUTINE OUTPUT_STATE


SUBROUTINE OUTPUT_STEP
	USE GLOBALS
	IMPLICIT NONE
	LOGICAL :: exist

	1000 FORMAT (i4, 1x, 5(f14.12,1x))

	l1 = SUM(dx(nl:nr)*ABS((c(nl:nr)-c0(nl:nr))))
    l2 = SQRT(SUM(dx(nl:nr)*(c(nl:nr)-c0(nl:nr))**2))
    tvd = SUM(ABS(c-c0))

    ! OPEN UP NORMS/TVD/TIME DATA FILE
    CALL WRITE_FILENAME( "../data/norm_tvd_time" )

    INQUIRE(file=filename,exist=exist)
    IF (exist) THEN
    	OPEN(UNIT=21,FILE=filename,STATUS="OLD",POSITION="APPEND",ACTION="WRITE")
    ELSE
    	OPEN(UNIT=21,FILE=filename,STATUS="NEW",ACTION="WRITE")
    ENDIF
    WRITE(21,1000) step, l1, l2, tvd, t
    CLOSE(UNIT=21)
    RETURN
END SUBROUTINE OUTPUT_STEP

SUBROUTINE WRITE_FILENAME( str )
	USE GLOBALS
	IMPLICIT NONE
	CHARACTER(LEN=*) :: str
	IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
		WRITE(filename, '(A,"_",A,"_monotonized_n",I0.4)') TRIM(str), TRIM(interpflag), nx
	ELSE
		WRITE(filename, '(A,"_",A,"_n",I0.4)') TRIM(str), TRIM(interpflag), nx
	ENDIF
END SUBROUTINE WRITE_FILENAME