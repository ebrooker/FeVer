SUBROUTINE get_parameterREAL (var_str, val_str, var_name, var_defval)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: var_str, val_str
    REAL, OPTIONAL, INTENT(IN)   :: var_defval
    REAL, INTENT(OUT)            :: var_name
    INTEGER                      :: io_read

    READ(val_str,*,iostat=io_read) var_name
    IF ( io_read .ne. 0 ) THEN
        ! CALL warning_ConversionFromString (  var_str, val_str, io_read )
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL warning_DefaultValue ( var_str )
        ELSE
            CALL warning_InitParameterFail( var_str )
        ENDIF
    ENDIF
END SUBROUTINE get_parameterREAL

SUBROUTINE get_parameterINTEGER (var_str, val_str, var_name, var_defval)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: var_str, val_str
    INTEGER, OPTIONAL, INTENT(IN) :: var_defval
    INTEGER, INTENT(OUT)          :: var_name
    INTEGER                       :: io_read

    READ(val_str,*,iostat=io_read) var_name
    IF ( io_read .ne. 0 ) THEN
        ! CALL warning_ConversionFromString (  var_str, val_str, io_read )
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL warning_DefaultValue ( var_str )
        ELSE
            CALL warning_InitParameterFail( var_str )
        ENDIF
    ENDIF
END SUBROUTINE get_parameterINTEGER

SUBROUTINE get_parameterCHARACTER (var_str, val_str, var_name, var_defval)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)           :: var_str, val_str
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: var_defval
    CHARACTER(LEN=*), INTENT(OUT)          :: var_name
    INTEGER                                :: io_read

    READ(val_str,*,iostat=io_read) var_name
    IF ( io_read .ne. 0 ) THEN
        ! CALL warning_ConversionFromString (  var_str, val_str, io_read )
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL warning_DefaultValue ( var_str )
        ELSE
            CALL warning_InitParameterFail( var_str )
        ENDIF
    ENDIF
END SUBROUTINE get_parameterCHARACTER

SUBROUTINE get_parameterLOGICAL (var_str, val_str, var_name, var_defval)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: var_str, val_str
    LOGICAL, OPTIONAL, INTENT(IN) :: var_defval
    LOGICAL, INTENT(OUT)          :: var_name
    INTEGER                       :: io_read

    READ(val_str,*,iostat=io_read) var_name
    IF ( io_read .ne. 0 ) THEN
        ! CALL warning_ConversionFromString (  var_str, val_str, io_read )
        IF ( PRESENT(var_defval) ) THEN
            var_name = var_defval
            CALL warning_DefaultValue ( var_str )
        ELSE
            CALL warning_InitParameterFail( var_str )
        ENDIF
    ENDIF
END SUBROUTINE get_parameterLOGICAL

!!------------------------!!
!!------------------------!!
!!--- warning routines ---!!
!!------------------------!!
!!------------------------!!

SUBROUTINE warning_ConversionFromString (var,val,stat)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: var, val
    INTEGER, INTENT(IN)          :: stat
    WRITE(*,*) "[WARNING]: CANNOT CONVERT val TO NON-STRING TYPE"
    WRITE(*,*) "           var:", TRIM(var)
    WRITE(*,*) "           val:", TRIM(val)
END SUBROUTINE warning_ConversionFromString

SUBROUTINE warning_DefaultValue ( str )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    WRITE(*,*) "           USING DEFAULT VALUE FOR ", TRIM(str)
    RETURN
END SUBROUTINE warning_DefaultValue

SUBROUTINE warning_InitParameterFail ( str )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: str
    WRITE(*,*) "           CANNOT INITIALIZE ", TRIM(str)
    WRITE(*,*) "           NOT FOUND IN INPUT PARAMETER FILE"
    WRITE(*,*) "           NO DEFAULT VALUE GIVEN"
    WRITE(*,*) "           [ABORTING SIMULATION]"
    STOP
END SUBROUTINE warning_InitParameterFail