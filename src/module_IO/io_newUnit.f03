MODULE io_newUnit

CONTAINS
    INTEGER FUNCTION newUnit(unit)
        INTEGER, INTENT(OUT), OPTIONAL :: unit

        INTEGER, PARAMETER :: UMIN=20, UMAX=1000
        INTEGER            :: UNUM
        LOGICAL                    :: OPENED
        
        newUnit = -1
        DO UNUM=UMIN,UMAX
            INQUIRE(UNIT=UNUM, OPENED=OPENED)
            IF (.not. OPENED) THEN
                newUnit=UNUM
                EXIT
            ENDIF
        ENDDO
        IF (PRESENT(unit)) unit=newUnit
    END FUNCTION newUnit

END MODULE io_newUnit