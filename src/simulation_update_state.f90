SUBMODULE (simulation_class) simulation_update_state

CONTAINS

    SUBROUTINE update_state(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this
        INTEGER(smInt)                     :: i

        DO CONCURRENT (i=1_smInt:this%nx)
            this%density%centers(i) = this%density%centers(i) &
                                      + this%dt * (this%density%flux(i) - this%density%flux(i+1)) / this%grid%dx(i)
        END DO

 
    END SUBROUTINE update_state

END SUBMODULE simulation_update_state