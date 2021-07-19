SUBMODULE (simulation_class) simulation_readInput

CONTAINS

    SUBROUTINE read_inputfile(this)
        CLASS(Simulation_t)           :: this
        INTEGER(lgInt)                :: error, io_err, io_unit, io_stat, nlines, iread
        CHARACTER(LEN=50)             :: key,val
        CHARACTER(LEN=1)              :: equal_string
        CHARACTER(LEN=:), ALLOCATABLE :: ikey,ival

        !! Use Python utility script to preprocess the parameter file for us
        CALL SYSTEM("python ./io_setupParameterFile.py "//TRIM(this%inputFile))

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
        this%parameters = Dictionary_t(nlines)
        WRITE(*,*) "[STARTUP] Creating input parameter dictionary"
        DO iread=1,nlines
            READ(io_unit,*) key, val
            ikey = TRIM(key)
            ival = TRIM(val)
            CALL this%parameters%addEntry(key=ikey, value=ival)
        ENDDO
        CLOSE(io_unit)

        CALL this%parameters%printEntries()
        CALL this%parameters%assignEntry(key="ng",      value=this%ng,      default_value =    4_smInt )
        CALL this%parameters%assignEntry(key="nx",      value=this%nx,      default_value =   11_smInt )
        CALL this%parameters%assignEntry(key="xmin",    value=this%xmin,    default_value =   -2.0_wp  )
        CALL this%parameters%assignEntry(key="xmax",    value=this%xmax,    default_value =    2.0_wp  )
        CALL this%parameters%assignEntry(key="tmax",    value=this%tmax,    default_value =    2.0_wp  )
        CALL this%parameters%assignEntry(key="cfl",     value=this%cfl,     default_value =    2.0_wp  )
        CALL this%parameters%assignEntry(key="bc_type", value=this%bc_type, default_value = "periodic" )
    
    END SUBROUTINE read_inputfile

END SUBMODULE simulation_readInput