MODULE io_newUnit
USE kind_settings

CONTAINS
    INTEGER(lgInt) FUNCTION newUnit(unit)
        INTEGER(lgInt), INTENT(OUT), OPTIONAL :: unit

        INTEGER(lgInt), PARAMETER :: UMIN=29, UMAX=299
        INTEGER(lgInt)            :: UNUM
        LOGICAL                   :: OPENED
        
        newUnit = -1_lgInt
        DO UNUM=UMIN,UMAX
            INQUIRE(UNIT=UNUM, OPENED=OPENED)
            IF (.not. OPENED) THEN
                newUnit = UNUM
                EXIT
            ENDIF
        ENDDO
        IF (PRESENT(unit)) unit=newUnit
    END FUNCTION newUnit

END MODULE io_newUnit