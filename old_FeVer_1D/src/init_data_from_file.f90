SUBROUTINE INIT_DATA_FROM_FILE_REAL( fname, var, stat)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname
    REAL(KIND=8),    INTENT(OUT) :: var(:)
    INTEGER(KIND=4), INTENT(OUT) :: stat
    OPEN(UNIT=24,FILE=TRIM(fname),STATUS="OLD",ACTION="READ")
    READ(24,*,iostat=stat) var(nl:nr)
    CLOSE(UNIT=24)
    RETURN
END SUBROUTINE INIT_DATA_FROM_FILE_REAL

SUBROUTINE INIT_DATA_FROM_FILE_INT( fname, var, stat)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER(KIND=4), INTENT(OUT) :: var(:)
    INTEGER(KIND=4), INTENT(OUT) :: stat
    OPEN(UNIT=24,FILE=TRIM(fname),STATUS="OLD",ACTION="READ")
    READ(24,*,iostat=stat) var(nl:nr)
    CLOSE(UNIT=24)
    RETURN
END SUBROUTINE INIT_DATA_FROM_FILE_INT

SUBROUTINE INIT_DATA_FROM_FILE_STR( fname, var, stat)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: fname
    CHARACTER(LEN=*), INTENT(OUT) :: var(:)
    INTEGER(KIND=4),  INTENT(OUT) :: stat
    OPEN(UNIT=24,FILE=TRIM(fname),STATUS="OLD",ACTION="READ")
    READ(24,*,iostat=stat) var(nl:nr)
    CLOSE(UNIT=24)
    RETURN
END SUBROUTINE INIT_DATA_FROM_FILE_STR

SUBROUTINE INIT_DATA_FROM_FILE_LOG( fname, var, stat)
    USE GLOBALS
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: fname
    LOGICAL,          INTENT(OUT) :: var(:)
    INTEGER(KIND=4),  INTENT(OUT) :: stat
    OPEN(UNIT=24,FILE=TRIM(fname),STATUS="OLD",ACTION="READ")
    READ(24,*,iostat=stat) var(nl:nr)
    CLOSE(UNIT=24)
    RETURN
END SUBROUTINE INIT_DATA_FROM_FILE_LOG
