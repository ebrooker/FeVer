MODULE simulation_class

USE kind_settings
use global_constants, ONLY : ZERO, ONE, TWO, HALF, PI

USE io_newUnit,          ONLY : newUnit

USE grid_class,          ONLY : Grid_t
USE dictionary_class,    ONLY : Dictionary_t
USE state_class,         ONLY : State_t

IMPLICIT NONE
PRIVATE


    PUBLIC :: Simulation_t

    TYPE :: Simulation_t

        TYPE(Dictionary_t)            :: parameters
        TYPE(Grid_t )                 :: grid
        TYPE(State_t)                 :: temp_state
        TYPE(State_t)                 :: temp_vel
        TYPE(State_t)                 :: density
        TYPE(State_t)                 :: velx

        INTEGER(smInt)                :: ng,nx

        REAL(wp)                      :: xmin,xmax
        REAL(wp)                      :: time=ZERO, dt=ZERO, tmax=ZERO, cfl=HALF

        CHARACTER(LEN=:), ALLOCATABLE :: inputfile
        CHARACTER(LEN=:), ALLOCATABLE :: bc_type

        CONTAINS
            PROCEDURE :: evolve  !! stub
            PROCEDURE :: init
            PROCEDURE :: init_data
            PROCEDURE :: init_grid
            PROCEDURE :: init_states
            PROCEDURE :: read_inputfile
            PROCEDURE :: reconstruction !! stub
            PROCEDURE :: timestep

    END TYPE Simulation_t

    INTERFACE Simulation_t
        PROCEDURE :: constructor
    END INTERFACE

    INTERFACE

        MODULE SUBROUTINE evolve(this)
            CLASS(Simulation_t), INTENT(INOUT) :: this
        END SUBROUTINE evolve


        MODULE SUBROUTINE init(this, inputfile)
            CLASS(Simulation_t)            :: this
            CHARACTER(LEN=*),   INTENT(IN) :: inputfile
        END SUBROUTINE init

        MODULE SUBROUTINE init_data(this)
            CLASS(Simulation_t), INTENT(INOUT) :: this
        END SUBROUTINE init_data

        MODULE SUBROUTINE init_grid(this)
            CLASS(Simulation_t) :: this
        END SUBROUTINE init_grid

        MODULE SUBROUTINE init_states(this)
            CLASS(Simulation_t) :: this
        END SUBROUTINE init_states


        MODULE SUBROUTINE boundary_conditions(values, ng, nx, bc_type)
            REAL(wp),         INTENT(INOUT) :: values(:)
            INTEGER(smInt),   INTENT(IN   ) :: ng,nx
            CHARACTER(LEN=*), INTENT(IN   ) :: bc_type

        END SUBROUTINE boundary_conditions

        MODULE SUBROUTINE get(nl, nr, state_vals, temp_vals)
            INTEGER(smInt), INTENT(IN   ) :: nl,nr
            REAL(wp),       INTENT(IN   ) :: state_vals(:)
            REAL(wp),       INTENT(INOUT) :: temp_vals(:)
        END SUBROUTINE get

        MODULE SUBROUTINE put(nl, nr, temp_vals, state_vals)
            INTEGER(smInt), INTENT(IN   ) :: nl,nr
            REAL(wp),       INTENT(IN   ) :: temp_vals(:)
            REAL(wp),       INTENT(INOUT) :: state_vals(:)
        END SUBROUTINE put

        MODULE SUBROUTINE reconstruction(this)
            CLASS(Simulation_t), INTENT(INOUT) :: this
        END SUBROUTINE

        MODULE SUBROUTINE timestep(this)
            CLASS(Simulation_t), INTENT(INOUT) :: this
        END SUBROUTINE timestep


        MODULE SUBROUTINE read_inputfile(this)
            CLASS(Simulation_t) :: this
        END SUBROUTINE read_inputfile



    END INTERFACE


CONTAINS

    FUNCTION constructor(inputfile) RESULT(this)
        CHARACTER(LEN=*), INTENT(IN) :: inputfile
        TYPE(Simulation_t)           :: this
        
        CALL this%init(inputfile)

    END FUNCTION constructor

END MODULE simulation_class