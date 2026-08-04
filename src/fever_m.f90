module fever
    use kinds_m
    use config_m
    use constants_m, only : pi, pi2

    ! use field_m, only : field_t
    use grid_m, only : grid_t
    use state_m, only : state_t
    use io_m, only : write_state_csv
    use functions_1d_m, only : sine, square_pulse
    use boundary_conditions_m, only : apply_periodic_bc
    use time_integration_m, only : advance_euler_explicit, compute_dt
    implicit none

    public :: print_banner

contains

    subroutine print_banner(filename)
        character(len=:), allocatable, intent(in) :: filename
        
        !! Locals
        character(len=:), allocatable :: buffer
        integer :: io, ret_code, nbuf
        logical :: exists

        inquire(file=filename, exist=exists)
        if (.not. exists) then
            write(*,"(2A)") "Banner file does not exist: ", filename
        end if

        open(newunit=io, file=filename, access="stream", action="read", form="unformatted", iostat=ret_code)

        if (ret_code /= 0) stop "Error: opening file failed - " // filename

        inquire (io, size=nbuf)
        allocate(character(len=nbuf) :: buffer)

        read(io, iostat=ret_code) buffer
        write(*,"(A)") buffer

        close(io)

    end subroutine print_banner
end module fever
