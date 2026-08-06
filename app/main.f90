program main
    use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
    use fever

    implicit none

    type(config_t) :: cfg
    type(grid_t) :: grid
    type(state_t) :: state

    GREETING: block
        character(len=:), allocatable :: banner_filepath
        banner_filepath = "resources/banners/fever-0.txt"
        call print_banner(banner_filepath)
        write(*,"(/,A)") compiler_version()
        write(*,"(/,A,/)") compiler_options()
    end block GREETING

    call read_config("/Users/ezrabrooker/Projects/WhiteSands/Fortran/FeVer/fever.nml", cfg)

    call grid%initialize(n_cells=cfg%n_cells, x_min=cfg%x_min, x_max=cfg%x_max, n_ghost=cfg%n_ghost)

    call state%initialize(n_vars=cfg%n_vars, grid=grid)

    !! sine wave
    state%u(1,1:grid%n_cells) = sine(grid%xc(1:grid%n_cells))

    !! square pulse
    state%u(1,1:grid%n_cells) = square_pulse(grid%xc(1:grid%n_cells), 1.0_rp/3.0_rp,2.0_rp/3.0_rp)

    call write_state_csv("data/old.csv", grid, state)

    write(*,"(DT)") grid

    EVOLVE: block
        real(rp) :: advection_speed
        real(rp) :: cfl
        real(rp) :: t_max
        real(rp) :: t, dt
        integer(ip) :: steps

        advection_speed = cfg%advection_speed
        cfl = cfg%cfl
        t_max = cfg%t_max
        t = 0.0_rp

        steps = 0
        do while(t < t_max)

            !! compute dt and limit to max time if needed
            dt = compute_dt(grid%dx, advection_speed, cfl)
            dt = min(dt, t_max-t)

            !! Apply BCs
            call apply_periodic_bc(grid=grid, state=state)

            ! call advance_euler_explicit(u, grid%n_cells, grid%n_ghost, grid%dx, dt, advection_speed)
            call advance_euler_explicit(grid, state, dt, advection_speed)

            !! Update time
            t = t + dt
            steps = steps + 1

        end do

        call write_state_csv("data/new.csv", grid, state)

    end block EVOLVE

end program main
