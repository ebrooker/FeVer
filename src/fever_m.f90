module fever
    use kinds_m
    use config_m
    use io_m
    use constants_m
    use grid_m
    use state_m
    use functions_1d_m
    use boundary_conditions_m, only : select_boundary_condition, bc_procedure_i
    use time_integration_m, only : select_integrator_method, select_rhs_method, integrator_procedure_i, rhs_procedure_i, compute_dt, compute_dt_burgers
    use reconstruct_m, only : select_reconstruction_method, reconstruction_procedure_i
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
