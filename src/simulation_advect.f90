SUBMODULE (simulation_class) simulation_advect

CONTAINS

    MODULE SUBROUTINE advection(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this
        INTEGER(smInt) :: nl, nr

        nl = this%ng + 1_smInt
        nr = this%ng + this%nx

        CALL this%temp_state%advect(nl,nr,this%grid%area,this%temp_vel%centers)

    END SUBROUTINE advection

END SUBMODULE simulation_advect