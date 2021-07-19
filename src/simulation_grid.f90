SUBMODULE (simulation_class) simulation_grid

CONTAINS

    SUBROUTINE init_grid(this)
        CLASS(Simulation_t) :: this
        this%grid = Grid_t(this%nx,this%xmin,this%xmax)
        CALL this%grid%info()
    END SUBROUTINE init_grid

END SUBMODULE simulation_grid