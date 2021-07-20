SUBMODULE (simulation_class) simulation_timestep

CONTAINS

    SUBROUTINE timestep(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this

        REAL(wp) :: dt_hydro
        REAL(wp) :: dt_diffusion
        dt_hydro     = HUGE(ONE)
        dt_diffusion = HUGE(ONE)

        dt_hydro = this%cfl * MIN( MINVAL(this%grid%dx(:)/this%velx%centers(:)), dt_hydro )

        this%dt = MINVAL( [abs(this%tmax-this%time), dt_hydro, dt_diffusion] )

    END SUBROUTINE timestep

END SUBMODULE simulation_timestep