SUBMODULE (simulation_class) simulation_evolve

CONTAINS

    SUBROUTINE evolve(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this

        INTEGER(smInt) :: i, nl, nr

        nl = this%ng + 1
        nr = this%ng + this%nx

        CALL this%timestep()

        CALL get(nl, nr, this%velx%centers, this%temp_vel%centers)
        CALL boundary_conditions(this%temp_vel%centers, this%ng, this%nx, "periodic")
        CALL this%reconstruction()
        CALL this%advection()
        CALL put(nl, nr, this%temp_state%flux, this%density%flux)
        


    END SUBROUTINE evolve

END SUBMODULE simulation_evolve