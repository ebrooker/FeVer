module timestep_m
    use kinds_m, only : rp, ip
    use grid_m, only : grid_t
    use state_m, only : state_t
    implicit none
    private
    public :: compute_dt, compute_dt_burgers

contains

    pure function compute_dt(dx, a, cfl) result(dt)
        real(rp), intent(in) :: dx, a, cfl
        real(rp) :: dt
        dt = CFL * dx / abs(a)
    end function compute_dt


    pure function compute_dt_burgers(grid, state, cfl) result(dt)
        type(grid_t), intent(in) :: grid
        type(state_t), intent(in) :: state
        real(rp), intent(in) :: cfl
        real(rp) :: dt
        dt = cfl * grid%dx / maxval(abs(state%u))
    end function compute_dt_burgers

end module timestep_m