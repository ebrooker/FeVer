SUBMODULE (simulation_class) simulation_evolve

CONTAINS

    SUBROUTINE evolve(this)
        CLASS(Simulation_t), INTENT(INOUT) :: this
        INTEGER(smInt)                     :: nl,nr,nr1
        INTEGER :: i
        INTEGER(lgInt) :: unt

        nl  = this%ng + 1_smInt
        nr  = this%ng + this%nx
        nr1 =      nr + 1_smInt
        
        ! Get guard cell temp_vel state variable for use with temp_state variable
        CALL get(nl, nr, this%velx%centers, this%temp_vel%centers)
       
        CALL boundary_conditions(this%temp_vel%centers, this%ng, this%nx, "periodic")


        DO WHILE (      this%time < this%tmax     &
                   .or. this%step < this%MAXSTEPS )

            ! Get timestep
            CALL this%timestep()

            ! Setup temp_state slice with guard cells and set boundary conditions
            CALL get(nl, nr, this%density%centers, this%temp_state%centers)
            CALL boundary_conditions(this%temp_state%centers, this%ng, this%nx, "periodic")

            ! Reconstruct left and right interfaces, then store in density state
            CALL this%temp_state%reconstruct(nl, nr, "plm", this%grid%dx, this%dt, this%temp_vel%centers)
            CALL put(nl, nr1, this%temp_state%intrfc_l, this%density%intrfc_l)            
            CALL put(nl, nr1, this%temp_state%intrfc_r, this%density%intrfc_r)

            ! Advect and store fluxes in density state
            CALL this%temp_state%advect(nl,nr1,this%grid%area,this%temp_vel%centers)
            CALL put(nl, nr1, this%temp_state%flux, this%density%flux)
            
            CALL this%update_state()
            ! CALL put(nl, nr, this%temp_state%centers, this%density%centers)

            this%time = this%time + this%dt
            this%step = this%step + 1_lgInt
            unt = this%step + 999_lgInt
            write(*,"(a,4es12.2)") "tmax,t,dt,cfl", this%tmax, this%time, this%dt, this%cfl
            ! write(*,*)this%density%centers
            write(unt,*)this%density%centers
            close(unt)
        END DO            

    END SUBROUTINE evolve

END SUBMODULE simulation_evolve