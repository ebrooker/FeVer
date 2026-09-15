program main_burgers
    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
    use fever
    use time_integration_m, only : compute_rhs_godunov_burgers, compute_rhs_rusanov_burgers, advance_burgers_explicit

    implicit none

    type(config_t) :: cfg
    type(grid_t) :: grid
    type(state_t) :: state
    character(len=:), allocatable :: chkpoint_basename, chkpoint_filename

    procedure(bc_procedure_i), pointer :: fill_ghost_cells => null()
    procedure(reconstruction_procedure_i), pointer :: reconstruction => null()
    procedure(integrator_procedure_i), pointer :: integrator => null()

    GREETING: block
        character(len=:), allocatable :: banner_filepath
        banner_filepath = "resources/banners/fever-0.txt"
        call print_banner(banner_filepath)
        write(*,"(/,A)") compiler_version()
        write(*,"(/,A,/)") compiler_options()
    end block GREETING

    call read_config("examples/burgers.nml", cfg)


    integrator => select_integrator_method(cfg%integrator_type)
    reconstruction => select_reconstruction_method(cfg%reconstruction_type)
    fill_ghost_cells => select_boundary_condition(cfg%bc_type)


    call grid%initialize(n_cells=cfg%n_cells, x_min=cfg%x_min, x_max=cfg%x_max, n_ghost=cfg%n_ghost)

    call state%initialize(n_vars=cfg%n_vars, grid=grid)

    !! Sod shock tube
    state%u(:,1:grid%n_cells/2) = -1.0
    state%u(:,grid%n_cells/2:) = 1.0

    state%u(:,1:grid%n_cells) = spread(sine(grid%xc), 1, 2)
    ! state%u(:,1:grid%n_cells) = spread(square_pulse(grid%xc, 0.4_rp,0.6_rp), 1, 2)

    
    chkpoint_basename = trim(cfg%base_name)
    chkpoint_filename = snapshot_filename(chkpoint_basename, 0)
    call write_state_csv(chkpoint_filename, grid, state)

    write(*,"(DT)") grid

    EVOLVE: block
        real(rp) :: cfl
        real(rp) :: t_max
        real(rp) :: t, dt
        integer(ip) :: steps

        cfl = cfg%cfl
        t_max = cfg%t_max
        t = 0.0_rp
        steps = 0

        do while(t < t_max)

            !! compute dt and limit to max time if needed
            dt = compute_dt_burgers(grid, state, cfl)
            dt = min(dt, t_max-t)

            call fill_ghost_cells(grid, state%u)

            call advance_burgers_explicit(grid, state, dt, cfg%flux_type, fill_ghost_cells, reconstruction)

            !! Update time
            t = t + dt
            steps = steps + 1

            if (steps > 1000) error stop "Over step count"

            chkpoint_filename = snapshot_filename(chkpoint_basename, steps)
            call write_state_csv(chkpoint_filename, grid, state)

        end do

    end block EVOLVE

end program main_burgers
