SUBROUTINE INIT_PARAMETER_getREAL (lines, var_str, var_name, var_defval)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)       :: lines(:,:), var_str
    REAL(KIND=8), OPTIONAL, INTENT(IN) :: var_defval
    REAL(KIND=8), INTENT(OUT)          :: var_name
    INTEGER(KIND=4)                    :: counter, io

    counter = nlines
    DO WHILE ( counter > 0 )
        IF ( TRIM(var_str) .eq. TRIM(lines(counter,1)) ) THEN
            CALL STR2REAL ( lines(counter,2), var_name, io )
            IF ( io .ne. 0 ) THEN
                CALL CONVERSION_WARNING ( lines(counter,2), var_str, io )
            ENDIF
            counter = -5
        ELSE
            counter = counter - 1
        ENDIF
    ENDDO

    IF ( counter .eq. 0 ) THEN
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL DEFAULT_WARNING ( var_str )
        ELSE
            CALL GET_WARNING( var_str )
        ENDIF
    ENDIF
    RETURN
END SUBROUTINE INIT_PARAMETER_getREAL

SUBROUTINE INIT_PARAMETER_getINT (lines, var_str, var_name, var_defval)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)          :: lines(:,:), var_str
    INTEGER(KIND=4), OPTIONAL, INTENT(IN) :: var_defval
    INTEGER(KIND=4), INTENT(OUT)          :: var_name
    INTEGER(KIND=4)                       :: counter, io

    counter = nlines
    DO WHILE ( counter > 0 )
        IF ( TRIM(var_str) .eq. TRIM(lines(counter,1)) ) THEN
            CALL STR2INT ( lines(counter,2), var_name, io )
            IF ( io .ne. 0 ) THEN
                CALL CONVERSION_WARNING ( lines(counter,2), var_str, io )
            ENDIF
            counter = -5
        ELSE
            counter = counter - 1
        ENDIF
    ENDDO

    IF ( counter .eq. 0 ) THEN
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL DEFAULT_WARNING ( var_str )
        ELSE
            CALL GET_WARNING( var_str )
        ENDIF
    ENDIF
    RETURN
END SUBROUTINE INIT_PARAMETER_getINT

SUBROUTINE INIT_PARAMETER_getSTR (lines, var_str, var_name, var_defval)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)           :: lines(:,:), var_str
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: var_defval
    CHARACTER(LEN=*), INTENT(OUT)          :: var_name
    INTEGER(KIND=4)                        :: counter

    counter = nlines
    DO WHILE ( counter > 0 )
        IF ( TRIM(var_str) .eq. TRIM(lines(counter,1)) ) THEN
            var_name = TRIM( lines(counter,2) )
            counter = -5
        ELSE
            counter = counter - 1
        ENDIF
    ENDDO

    IF ( counter .eq. 0 ) THEN
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL DEFAULT_WARNING ( var_str )
        ELSE
            CALL GET_WARNING( var_str )
        ENDIF
    ENDIF
    RETURN
END SUBROUTINE INIT_PARAMETER_getSTR

SUBROUTINE INIT_PARAMETER_getLOG (lines, var_str, var_name, var_defval)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: lines(:,:), var_str
    LOGICAL, OPTIONAL, INTENT(IN) :: var_defval
    LOGICAL, INTENT(OUT)          :: var_name
    INTEGER(KIND=4)               :: counter, io

    counter = nlines
    DO WHILE ( counter > 0 )

        IF ( TRIM(var_str) .eq. TRIM(lines(counter,1)) ) THEN
            CALL STR2LOG ( lines(counter,2), var_name, io )
            IF ( io .ne. 0 ) THEN
                CALL CONVERSION_WARNING ( lines(counter,2), var_str, io )
            ENDIF
            counter = -5
        ELSE
            counter = counter - 1
        ENDIF
    ENDDO

    IF ( counter .eq. 0 ) THEN
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL DEFAULT_WARNING ( var_str )
        ELSE
            CALL GET_WARNING( var_str )
        ENDIF
    ENDIF
    RETURN
END SUBROUTINE INIT_PARAMETER_getLOG
!!---------------------------------------------------------------------

!!----------------------------------!!
!!--- string to type conversions ---!!
!!----------------------------------!!

SUBROUTINE STR2REAL (str,val,stat)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    REAL(KIND=8), INTENT(OUT)    :: val
    INTEGER(KIND=4), INTENT(OUT) :: stat
    READ(str,*,iostat=stat) val
    RETURN
END SUBROUTINE STR2REAL

SUBROUTINE STR2INT (str,val,stat)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER(KIND=4), INTENT(OUT) :: Val
    INTEGER(KIND=4), INTENT(OUT) :: stat
    READ(str,*,iostat=stat) val
    RETURN
END SUBROUTINE STR2INT

SUBROUTINE STR2LOG (str,val,stat)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    LOGICAL, INTENT(OUT)         :: val
    INTEGER(KIND=4), INTENT(OUT) :: stat
    READ(str,*,iostat=stat) val
    RETURN
END SUBROUTINE STR2LOG
!!---------------------------------------------------------------------

!!------------------------!!
!!--- warning routines ---!!
!!------------------------!!

SUBROUTINE CONVERSION_WARNING (str,val,stat)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str, val
    INTEGER(KIND=4), INTENT(IN)  :: stat
    PRINT *, "[WARNING]: CANNOT CONVERT val TO NON-STRING TYPE"
    PRINT *, "         str:", TRIM(str)
    PRINT *, "         val:", TRIM(str)
    PRINT *, "         [ABORTING SIMULATION]"
    STOP
END SUBROUTINE CONVERSION_WARNING

SUBROUTINE DEFAULT_WARNING ( str )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    PRINT *, "[WARNING]: USING DEFAULT VALUE FOR ", TRIM(str)
    RETURN
END SUBROUTINE DEFAULT_WARNING

SUBROUTINE GET_WARNING ( str )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    PRINT *, "[WARNING]: CANNOT INITIALIZE ", TRIM(str)
    PRINT *, "         NOT FOUND IN INPUT PARAMETER FILE"
    PRINT *, "         NO DEFAULT VALUE GIVEN"
    PRINT *, "         [ABORTING SIMULATION]"
    STOP
END SUBROUTINE GET_WARNING