MODULE grid_class
USE kind_settings
USE global_constants
IMPLICIT NONE
PRIVATE

    PUBLIC :: Grid_t

    TYPE :: Grid_t

        INTEGER(smInt)        :: nx
        REAL(wp)              :: xmin
        REAL(wp)              :: xmax
        REAL(wp), ALLOCATABLE :: xi(:), xc(:), dx(:)
        REAL(wp), ALLOCATABLE :: area(:), vol(:)

        CONTAINS
            PROCEDURE :: init
            PROCEDURE :: info

    END TYPE Grid_t

    INTERFACE Grid_t
        PROCEDURE :: constructor
    END INTERFACE

CONTAINS

    FUNCTION constructor(nx, xmin, xmax) RESULT(this)
        TYPE(Grid_t)               :: this
        INTEGER(smInt), INTENT(IN) :: nx
        REAL(wp),       INTENT(IN) :: xmin, xmax

        CALL this%init(nx,xmin,xmax)

    END FUNCTION constructor        

    SUBROUTINE init(this,nx,xmin,xmax)
        CLASS(Grid_t)              :: this
        INTEGER(smInt), INTENT(IN) :: nx
        REAL(wp),       INTENT(IN) :: xmin, xmax
        INTEGER(smInt)             :: i

        this%nx   = nx
        this%xmin = xmin
        this%xmax = xmax

        IF (ALLOCATED(this%xi  )) DEALLOCATE(this%xi  )
        IF (ALLOCATED(this%xc  )) DEALLOCATE(this%xc  )
        IF (ALLOCATED(this%dx  )) DEALLOCATE(this%dx  )
        IF (ALLOCATED(this%area)) DEALLOCATE(this%area)
        IF (ALLOCATED(this%vol )) DEALLOCATE(this%vol )

        ALLOCATE(this%xi  (this%nx+1_smInt), source=ZERO)
        ALLOCATE(this%xc  (this%nx        ), source=ZERO)
        ALLOCATE(this%dx  (this%nx        ), source=ZERO)
        ALLOCATE(this%area(this%nx+1_smInt), source=ZERO)
        ALLOCATE(this%vol (this%nx        ), source=ZERO)

        this%dx   = (this%xmax-this%xmin) / real(this%nx, kind=wp)
        this%xi   = [ (this%xmin + i*this%dx(i), i=0,this%nx-1)      ]
        this%xc   = [ (0.5e0*(this%xi(i)+this%xi(i+1)), i=1,this%nx) ]
        this%area = this%xi**2
        this%vol  = this%xc**2 * this%dx

    END SUBROUTINE init

    SUBROUTINE info(this)
        CLASS(Grid_t), INTENT(IN) :: this

        write(*,"(A)"          ) "#"//REPEAT("-",21)//"#"
        write(*,"(A)"          ) "#"//REPEAT("-", 5)//" Grid Info "//REPEAT("-", 5)//"#"
        write(*,"(A)"          ) "#"//REPEAT("-",21)//"#"
        write(*,"(A,I10,A3)"   ) "#  nx   = ", this%nx,      "#"
        write(*,"(A,ES10.4,A3)") "#  xmin = ", this%xmin,    "#"
        write(*,"(A,ES10.4,A3)") "#  xmax = ", this%xmax,    "#"
        write(*,"(A,ES10.4,A3)") "#  delx = ", this%dx(1),   "#"
        write(*,"(A)"          ) "#"//REPEAT("-",21)//"#"
    END SUBROUTINE info

END MODULE grid_class