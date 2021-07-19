SUBMODULE (simulation_class) simulation_data

CONTAINS

    SUBROUTINE init_data(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this


        this%density%centers = SIN(TWO*PI*this%grid%xc)
        this%velx%centers    = ONE

    END SUBROUTINE

END SUBMODULE simulation_data