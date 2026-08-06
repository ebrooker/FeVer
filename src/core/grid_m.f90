module grid_m
    use kinds_m, only : rp, ip
    implicit none
    private

    public :: grid_t

    type :: grid_t
    !!
    !! The cell centers and interfaces are ordered as follows,
    !!
    !! x_min                         x_max
    !! |----.----|----.----|----.----|
    !! i0   c1   i1   c2   i2   c3   i3
    !!
    !! where, i[0-3] are interfaces 0, 1, 2, and 3 and c[1-3]
    !! are cell centers 1,2, and 3. The minimum of the range
    !! is i0 = x_min and the maximum is i3 = x_max.
    !!
        integer(ip) :: n_cells
        integer(ip) :: n_ghost = 1_ip
        integer(ip) :: ilo, ihi
        real(rp) :: dx, x_min, x_max
        real(rp) :: cell_area, cell_volume
        real(rp), allocatable :: xc(:)  ! cell centers, size 1-n_ghost:n_cells+n_ghost
        real(rp), allocatable :: xi(:)  ! cell intrfcs, size 0-n_ghost:n_cells+n_ghost

    contains
        procedure :: write_grid
        generic   :: write(formatted) => write_grid
        procedure :: initialize => grid_initialize
        procedure :: clear => grid_clear
    end type grid_t

contains

    subroutine write_grid(this, unit, iotype, v_list, iostat, iomsg)
        class(grid_t), intent(in) :: this
        integer, intent(in) :: unit, v_list(:)
        character(len=*), intent(in) :: iotype
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        integer :: i, sh(2)

        select case (iotype)
        ! case ('listdirected')
        !     write(unit, fmt=*, delim='quote', iostat=iostat, iomsg=iomsg) &
        !     dtv%data%string
        ! case ('namelist')
        !     write(unit, fmt=*, iostat=iostat, iomsg=iomsg) '"', &
        !     dtv%data%string, '",', trim(next_component)
        case default
            write(unit, "(A)") "grid_t("
            write(unit, "(A,I0,A)") "n_cells=",this%n_cells, ", "
            write(unit, "(A,I0,A)") "n_ghost=",this%n_ghost, ", "
            write(unit, "(A,E10.4,A)") "x_min=",this%x_min, ", "
            write(unit, "(A,E10.4,A)") "x_max=",this%x_max, ", "
            write(unit, "(A,E10.4)") "dx=",this%dx
            write(unit, "(A)") ")"
        end select

        return
    end subroutine write_grid

    subroutine grid_initialize(this, n_cells, x_min, x_max, n_ghost)
        class(grid_t), intent(inout) :: this
        integer(ip), intent(in) :: n_cells
        real(rp), intent(in) :: x_min, x_max
        integer(ip), intent(in), optional :: n_ghost

        integer(ip) :: i, ilo, ihi

        this%n_cells = n_cells
        this%x_min = x_min
        this%x_max = x_max
        this%dx = (this%x_max - this%x_min) / (real(this%n_cells, rp))

        !! Optionally set ghost cells, default is 1
        if (present(n_ghost)) then
            this%n_ghost = n_ghost
        else
            this%n_ghost = 1
        end if

        !! Set ghost cell adjust lower and upper bounds
        this%ilo = 1_ip-this%n_ghost
        this%ihi = n_cells+this%n_ghost

        !! Allocate and fill the cell centers and faces
        call this%clear()
        allocate(this%xc(this%ilo:this%ihi), source=0.0_rp)
        allocate(this%xi(this%ilo-1:this%ihi), source=0.0_rp)

        this%xi = [ (this%x_min + i*this%dx, i=this%ilo-1,this%ihi) ]
        this%xc = 0.5_rp * (this%xi(this%ilo-1:this%ihi-1) + this%xi(this%ilo:this%ihi))

        this%cell_area = 1.0_rp
        this%cell_volume = this%dx * 1.0_rp * 1.0_rp

    end subroutine grid_initialize


    subroutine grid_clear(this)
        class(grid_t), intent(inout) :: this
        if (allocated(this%xc)) deallocate(this%xc)
        if (allocated(this%xi)) deallocate(this%xi)
    end subroutine grid_clear    

end module grid_m