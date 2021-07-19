SUBMODULE (simulation_class) simulation_states

CONTAINS

    SUBROUTINE init_states(this)
        CLASS(Simulation_t) :: this
        
        this%temp_state = State_t( name = "Temp State",            &
                                   nx   = this%nx+2_smInt*this%ng, &
                                   nxi  = this%nx+2_smInt*this%ng, &
                                   nxf  = this%nx+2_smInt*this%ng  )

        this%temp_vel   = State_t( name = "Temp Vel",              &
                                   nx   = this%nx                  )


        this%density    = State_t( name = "Density",               &
                                   nx   = this%nx,                 &
                                   nxf  = this%nx+1_smInt          )

        this%velx       = State_t( name = "Velocity  x",           &
                                   nx   = this%nx                  )

    END SUBROUTINE init_states

END SUBMODULE simulation_states