MODULE get_parameterInterface

    IMPLICIT NONE

    INTERFACE get_parameter

        SUBROUTINE get_parameterREAL (var_str, val_str, var_name, var_defval)
        CHARACTER(LEN=*), INTENT(IN) :: var_str, val_str
        REAL, OPTIONAL, INTENT(IN)   :: var_defval
        REAL, INTENT(OUT)            :: var_name
        END SUBROUTINE get_parameterREAL

        SUBROUTINE get_parameterINTEGER (var_str, val_str, var_name, var_defval)
        CHARACTER(LEN=*), INTENT(IN)  :: var_str, val_str
        INTEGER, OPTIONAL, INTENT(IN) :: var_defval
        INTEGER, INTENT(OUT)          :: var_name
        END SUBROUTINE get_parameterINTEGER

        SUBROUTINE get_parameterCHARACTER (var_str, val_str, var_name, var_defval)
        CHARACTER(LEN=*), INTENT(IN)           :: var_str, val_str
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: var_defval
        CHARACTER(LEN=*), INTENT(OUT)          :: var_name
        END SUBROUTINE get_parameterCHARACTER

        SUBROUTINE get_parameterLOGICAL (var_str, val_str, var_name, var_defval)
        CHARACTER(LEN=*), INTENT(IN)  :: var_str, val_str
        LOGICAL, OPTIONAL, INTENT(IN) :: var_defval
        LOGICAL, INTENT(OUT)          :: var_name
        END SUBROUTINE get_parameterLOGICAL

    END INTERFACE get_parameter

END MODULE get_parameterInterface