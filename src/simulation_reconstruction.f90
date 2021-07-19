SUBMODULE (simulation_class) simulation_reconstruction

CONTAINS

    SUBROUTINE reconstruction(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this
        INTEGER(smInt) :: i, nl, nr

        nl = this%ng + 1
        nr = this%ng + this%nx

        CALL get(nl, nr, this%density%centers, this%temp_state%centers)

        CALL boundary_conditions(this%temp_state%centers, this%ng, this%nx, "periodic")

        CALL this%temp_state%reconstruct(nl, nr, "plm", this%grid%dx, this%dt, this%velx%centers)

        CALL put(nl, nr+1_smInt, this%temp_state%intrfc_l, this%density%intrfc_l)            
        CALL put(nl, nr+1_smInt, this%temp_state%intrfc_r, this%density%intrfc_r)

    END SUBROUTINE reconstruction

END SUBMODULE simulation_reconstruction