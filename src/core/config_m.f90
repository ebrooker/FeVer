module config_m
  !! Namelist-based configuration reader for the gas dynamics solver.
  use kinds_m, only: rp
  implicit none
  private
  public :: config_t, read_config

  type :: config_t
    integer  :: n_cells
    integer  :: n_ghost
    integer  :: n_vars
    real(rp) :: x_min
    real(rp) :: x_max
    real(rp) :: advection_speed
    real(rp) :: cfl
    real(rp) :: t_max
  end type config_t

contains

  subroutine read_config(filename, cfg)
    character(len=*), intent(in)  :: filename
    type(config_t),   intent(out) :: cfg

    integer            :: unit, ios
    character(len=256) :: iomsg

    integer  :: n_cells, n_ghost, n_vars
    real(rp) :: x_min, x_max, advection_speed, cfl, t_max

    namelist /grid_nml/ n_cells, n_ghost, n_vars, &
                         x_min, x_max, advection_speed, cfl, t_max

    ! Defaults -- used for any entry the namelist file omits
    n_cells = 100
    n_ghost = 2
    n_vars  = 1
    x_min = 0.0_rp
    x_max = 1.0_rp
    advection_speed = 1.0_rp
    cfl   = 0.5_rp
    t_max = 1.0_rp

    open (newunit=unit, file=filename, status='old', action='read', &
          iostat=ios, iomsg=iomsg)
    if (ios /= 0) error stop &
      'read_config: cannot open "'//trim(filename)//'": '//trim(iomsg)

    read (unit, nml=grid_nml, iostat=ios, iomsg=iomsg)
    if (ios /= 0) error stop &
      'read_config: bad &grid_nml in "'//trim(filename)//'": '//trim(iomsg)

    close (unit)

    cfg = config_t(n_cells, n_ghost, n_vars, x_min, x_max, &
                    advection_speed, cfl, t_max)

  end subroutine read_config

end module config_m