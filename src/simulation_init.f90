SUBMODULE (simulation_class) simulation_init

CONTAINS

    SUBROUTINE init(this, inputfile)
        CLASS(Simulation_t)            :: this
        CHARACTER(LEN=*),   INTENT(IN) :: inputfile

        this%inputfile = inputfile
        CALL this%read_inputfile()
        CALL this%init_grid()
        CALL this%init_states()
        CALL this%init_data()

        CALL this%density%info()
        CALL this%velx%info()

    END SUBROUTINE init

END SUBMODULE simulation_init