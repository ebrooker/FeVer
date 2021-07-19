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
            ! PROCEDURE :: norm
            ! PROCEDURE :: diff
            ! PROCEDURE :: mean
            ! PROCEDURE :: mag
            ! PROCEDURE :: cca


    END TYPE State_t

    INTERFACE State_t
        PROCEDURE :: constructor
    END INTERFACE State_t

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

    SUBROUTINE reconstruct(this,nl,nr,interp_type,dx,dt,velx)
        CLASS(State_t),     INTENT(INOUT) :: this
        INTEGER(smInt),     INTENT(IN   ) :: nl,nr
        CHARACTER(LEN=*),   INTENT(IN   ) :: interp_type
        REAL(wp), OPTIONAL, INTENT(IN   ) :: dx(:),dt,velx(:)
        INTEGER(smInt)                    :: i
        REAL(wp),           ALLOCATABLE   :: dtdx(:)
        REAL(wp),           ALLOCATABLE   :: slope(:)

        SELECT CASE (TRIM(interp_type))

        CASE("pcm")

            this%intrfc_l(nl:nr+1) = this%centers(nl-1:nr)
            this%intrfc_r(nl:nr+1) = this%centers(nl:nr+1)

        CASE("plm")

            IF (.NOT. PRESENT(dx) .OR. .NOT. PRESENT(dt) .OR. .NOT. PRESENT(velx)) &
                STOP "Optional Variables for PLM Reconstruction unavailable"

            IF (ALLOCATED(dtdx)) DEALLOCATE(dtdx)
            ALLOCATE(dtdx(SIZE(dx)), source=dt/dx(:))
            ! dtdx = dt/dx

            IF (ALLOCATED(slope)) DEALLOCATE(slope)
            ALLOCATE(slope(this%nx), source=ZERO)

            slope(nl-1:nr+1) = maxmod( minmod(     (this%centers(nl  :nr+2) - this%centers(nl-1:nr+1)),  &
                                               TWO*(this%centers(nl-1:nr+1) - this%centers(nl-2:nr  ))), &
                                       minmod( TWO*(this%centers(nl  :nr+2) - this%centers(nl-1:nr+1)),  &
                                                   (this%centers(nl-1:nr+1) - this%centers(nl-2:nr  )))  &
                                ) / dx(nl-1:nr+1) !! end maxmod

            this%intrfc_l(nl:nr+1) = this%centers(nl-1:nr) + &
                                     HALF*dx(nl-1:nr+1)*(ONE - velx(nl:nr+1)*dtdx(nl-1:nr+1))*slope(nl-1:nr)
            this%intrfc_r(nl:nr+1) = this%centers(nl:nr+1) + &
                                     HALF*dx(nl-1:nr+1)*(ONE + velx(nl:nr+1)*dtdx(nl-1:nr+1))*slope(nl:nr+1)

        CASE DEFAULT
            STOP "[RECONSTRUCTION] Unknown interpolation type"

        END SELECT


    END SUBROUTINE reconstruct


    ! ! Templates
    ! FUNCTION
    !     CLASS(State_t) :: this

    ! END FUNCTION

    ! SUBROUTINE
    !     CLASS(State_t) :: this

    ! END SUBROUTINE


    ELEMENTAL FUNCTION minmod(a,b)
        IMPLICIT NONE
        REAL(wp), INTENT(IN) :: a, b
        REAL(wp) :: minmod
        IF (ABS(a) < ABS(b) .AND. a*b > ZERO) THEN
            minmod = a
        ELSE IF (ABS(b) < ABS(a) .AND. a*b > ZERO) THEN
            minmod = b
        ELSE
            minmod = ZERO
        ENDIF
        RETURN
    END FUNCTION minmod

    !======================================================================!

    ELEMENTAL FUNCTION maxmod(a,b)
        IMPLICIT NONE
        REAL(wp), INTENT(IN) :: a, b
        REAL(wp) :: maxmod
        IF (ABS(a) > ABS(b) .AND. a*b > ZERO) THEN
            maxmod = a
        ELSE IF (ABS(b) > ABS(a) .AND. a*b > ZERO) THEN
            maxmod = b
        ELSE
            maxmod = ZERO
        ENDIF 
        RETURN
    END FUNCTION maxmod


END MODULE state_class