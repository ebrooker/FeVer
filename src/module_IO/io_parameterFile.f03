MODULE io_parameterFile
    USE fdict
    USE io_newUnit
    USE globals
    USE get_parameterInterface
    IMPLICIT NONE
    PUBLIC

    CONTAINS
        SUBROUTINE io_setupParameterFile
            IMPLICIT NONE
            CALL SYSTEM("python MODULE_IO/io_setupParameterFile.py test_data/test.par")
        END SUBROUTINE io_setupParameterFile


        SUBROUTINE io_readParameterFile
            USE fdict
            USE io_newunit
            IMPLICIT NONE
            TYPE(hash_tbl_sll) :: dict
            INTEGER   :: error, io_err, io_unit, io_stat, nlines, iread
            CHARACTER(LEN=50) :: key,val
            CHARACTER(LEN=1)  :: equal_string
            CHARACTER(LEN=:), ALLOCATABLE :: ikey,ival
            OPEN(UNIT=newUnit(io_unit), FILE="parameters.fever", STATUS="OLD", ACTION="READ", IOSTAT=io_err)

            nlines = 0
            DO
                READ(io_unit,*,iostat=io_stat)
                IF (io_stat .ne. 0) EXIT
                nlines = nlines + 1
            ENDDO
            CLOSE(io_unit)
            OPEN(UNIT=newUnit(io_unit), FILE="parameters.fever", STATUS="OLD", ACTION="READ", IOSTAT=io_err)

            WRITE(*,*) ""
            WRITE(*,*) "[SETUP] Initializing parameter dictionary"
            CALL dict%init(nlines)
            WRITE(*,*) "[SETUP] Reading parameters into dictionary"
            DO iread=1,nlines
                READ(io_unit,*) key, val
                ikey = TRIM(key)
                ival = TRIM(val)
                CALL dict%put(key=ikey, val=ival)
            ENDDO
            CLOSE(io_unit)
            WRITE(*,*) "[SETUP] Saving dictionary parameters to global startup variables"

            !
            ! Try to keep the getParameter statements grouped by first letter. They don't 
            ! have to be alphabetized or anything, just keep them grouped by Aa's, Bb's, etc
            ! to help make the read-in a little more efficient (the READER keeps track of
            ! where in file-lines the next letter grouping occurs to reduce some loop time)
            !

            CALL get_parameter("nx", dict%get("nx"), nx, 4)
            CALL get_parameter("nxb", dict%get("nxb"), nxb, 4)

            ! CALL getParameter('nx', nx, 8)
            ! CALL getParameter('tmax', tmax, 1.0)
            ! CALL getParameter('xmin', xmin, 0.0)
            ! CALL getParameter('xmax', xmax, 1.0)

            CALL dict%free()
        END SUBROUTINE io_readParameterFile

END MODULE io_parameterFile