module state_m
    use grid_m, only : grid_t
    use kinds_m, only : rp, ip
    implicit none
    private
    public :: state_t

    type :: state_t
        integer(ip)           :: n_vars  !! number of state variables -- not a compile time constant
        real(rp), allocatable :: u(:,:)

    contains
        procedure :: write_state
        generic   :: write(formatted) => write_state
        procedure :: initialize => state_initialize
        procedure :: clear => state_clear
    end type state_t

contains

    subroutine write_state(this, unit, iotype, v_list, iostat, iomsg)
        class(state_t), intent(in) :: this
        integer, intent(in) :: unit, v_list(:)
        character(len=*), intent(in) :: iotype
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        select case (iotype)
        ! case ('listdirected')
        !     write(unit, fmt=*, delim='quote', iostat=iostat, iomsg=iomsg) &
        !     dtv%data%string
        ! case ('namelist')
        !     write(unit, fmt=*, iostat=iostat, iomsg=iomsg) '"', &
        !     dtv%data%string, '",', trim(next_component)
        case default
            write(unit, "(A)") "state_t("
            write(unit, "(A,I0,A)") "n_vars=",this%n_vars, ", "
            write(unit, "(A)") ")"
        end select

        return
    end subroutine write_state

    subroutine state_initialize(this, n_vars, grid)
        class(state_t), intent(inout) :: this
        integer(ip), intent(in) :: n_vars
        type(grid_t), intent(in) :: grid

        this%n_vars = n_vars

        !! Allocate and fill the cell centers and faces
        call this%clear()
        allocate(this%u(this%n_vars, 1-grid%n_ghost:grid%n_cells+grid%n_ghost), source=0.0_rp)

    end subroutine state_initialize

    subroutine state_clear(this)
        class(state_t), intent(inout) :: this
        if (allocated(this%u)) deallocate(this%u)
    end subroutine state_clear    

end module state_m