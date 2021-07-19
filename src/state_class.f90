MODULE state_class
USE kind_settings, ONLY : smInt, wp
use global_constants, ONLY : ZERO, ONE, TWO, HALF
IMPLICIT NONE
PRIVATE

    PUBLIC :: State_t

    TYPE State_t
        CHARACTER(LEN=:), ALLOCATABLE :: name
        INTEGER(smInt)                :: nx, ni, nf
        REAL(wp),         ALLOCATABLE :: centers(:)
        REAL(wp),         ALLOCATABLE :: intrfc_l(:)
        REAL(wp),         ALLOCATABLE :: intrfc_r(:)
        REAL(wp),         ALLOCATABLE :: flux(:)

        CONTAINS
            PROCEDURE :: init
            PROCEDURE :: info
            PROCEDURE :: reconstruct
            PROCEDURE :: advect
            ! PROCEDURE :: norm
            ! PROCEDURE :: diff
            ! PROCEDURE :: mean
            ! PROCEDURE :: mag
            ! PROCEDURE :: cca


    END TYPE State_t

    INTERFACE State_t
        PROCEDURE :: constructor
    END INTERFACE State_t

    INTERFACE
        MODULE SUBROUTINE reconstruct(this,nl,nr,interp_type,dx,dt,velx)
            CLASS(State_t),     INTENT(INOUT) :: this
            INTEGER(smInt),     INTENT(IN   ) :: nl,nr
            CHARACTER(LEN=*),   INTENT(IN   ) :: interp_type
            REAL(wp), OPTIONAL, INTENT(IN   ) :: dx(:),dt,velx(:)
        END SUBROUTINE reconstruct

        MODULE SUBROUTINE advect(this, nl, nr, area, velocity)
            CLASS(State_t), INTENT(INOUT) :: this
            INTEGER(smInt), INTENT(IN   ) :: nl,nr
            REAL(wp),       INTENT(IN   ) :: area(:), velocity(:)
        END SUBROUTINE advect
    END INTERFACE

CONTAINS

    FUNCTION constructor(name,nx,nxi,nxf) RESULT(this)
        TYPE(State_t)                          :: this
        CHARACTER(LEN=*),           INTENT(IN) :: name
        INTEGER(smInt),             INTENT(IN) :: nx
        INTEGER(smInt),   OPTIONAL, INTENT(IN) :: nxi
        INTEGER(smInt),   OPTIONAL, INTENT(IN) :: nxf

        INTEGER(smInt) :: ni=1, nf=1

        IF ( PRESENT(nxi) ) ni = nxi
        IF ( PRESENT(nxf) ) nf = nxf    
        CALL this%init(name,nx,ni,nf)

    END FUNCTION constructor


    SUBROUTINE init(this, name, nx, ni, nf)
        CLASS(State_t)                         :: this
        CHARACTER(LEN=*),           INTENT(IN) :: name
        INTEGER(smInt),             INTENT(IN) :: nx, ni, nf


        this%name = name
        this%nx   = nx
        this%ni   = ni
        this%nf   = nf

        IF (ALLOCATED(this%centers)) DEALLOCATE(this%centers)
        ALLOCATE(this%centers(this%nx), source=ZERO)

        IF (ALLOCATED(this%intrfc_l)) DEALLOCATE(this%intrfc_l)
        ALLOCATE(this%intrfc_l(this%ni), source=ZERO)

        IF (ALLOCATED(this%intrfc_r)) DEALLOCATE(this%intrfc_r)
        ALLOCATE(this%intrfc_r(this%ni), source=ZERO)

        IF (ALLOCATED(this%flux)) DEALLOCATE(this%flux)
        ALLOCATE(this%flux(this%nf), source=ZERO)

    END SUBROUTINE init


    SUBROUTINE info(this)
        CLASS(State_t), INTENT(IN) :: this
        INTEGER(smInt) :: i
        WRITE(*,"(A)") "#"//REPEAT("-",21)//"#"
        WRITE(*,"(2A,X,A,X,2A)") "#", REPEAT("-",9-LEN(this%name)/2), this%name, REPEAT("-",9-LEN(this%name)/2), "#"
        DO i=1,this%nx
            WRITE(*,"(A,I5,ES14.4,A)") "# ", i, this%centers(i), " #"
        END DO

    END SUBROUTINE info


    ! ! Templates
    ! FUNCTION
    !     CLASS(State_t) :: this

    ! END FUNCTION

    ! SUBROUTINE
    !     CLASS(State_t) :: this

    ! END SUBROUTINE


END MODULE state_class