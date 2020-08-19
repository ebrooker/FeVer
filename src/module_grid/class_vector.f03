MODULE class_vector
    IMPLICIT NONE
    PRIVATE
    REAL, PARAMETER :: PI=4.0*ATAN(1.0)
    PUBLIC vector

    TYPE vector
        INTEGER           :: nvec
        REAL, ALLOCATABLE :: vec(:)
        CONTAINS
            PROCEDURE :: linspace => vector_linear_spacing
            PROCEDURE :: init     => vector_init
            PROCEDURE :: norm     => vector_norm
            PROCEDURE :: diff     => vector_difference
            PROCEDURE :: mag      => vector_magnitude
            PROCEDURE :: cca      => vector_cell_entered
            PROCEDURE :: del      => vector_delete

    END TYPE vector

    CONTAINS

        SUBROUTINE vector_init(this,nvec)
            CLASS(vector)       :: this
            INTEGER, INTENT(IN) :: nvec
            this%nvec = nvec
            IF (ALLOCATED(this%vec)) DEALLOCATE(this%vec)
            ALLOCATE(this%vec(this%nvec))
            this%vec = 0.0
        END SUBROUTINE vector_init

        SUBROUTINE vector_delete(this)
            CLASS(vector) :: this
            this%nvec = 0
            IF (ALLOCATED(this%vec)) DEALLOCATE(this%vec)
        END SUBROUTINE vector_delete

        FUNCTION vector_norm(this, ord) RESULT(val)
            CLASS(vector)    :: this
            CHARACTER(LEN=*) :: ord
            REAL             :: val
            INTEGER          :: p, io_read, ivec

            IF (TRIM(ord) .eq. 'inf' .or. TRIM(ord) .eq. 'INF' .or. TRIM(ord) .eq. 'Inf') THEN
                val = MAXVAL(this%vec, DIM=1)
            ELSE
                READ(ord,*,iostat=io_read) p
                IF (io_read .ne. 0) STOP
                val = 0.0
                ! val = SUM(this%vec**p)**(1.0/p)
                DO ivec = 1,this%nvec
                    val = val + ABS(this%vec(ivec))**p
                ENDDO
                val = val**(1.0/p)
            ENDIF
        END FUNCTION vector_norm

        FUNCTION vector_magnitude(this) RESULT(val)
            CLASS(vector) :: this
            REAL          :: val
            val = this%norm(ord='2')
        END FUNCTION vector_magnitude

        SUBROUTINE vector_linear_spacing(this,lower,upper)
            CLASS(vector) :: this
            REAL,  INTENT(IN) :: lower,upper
            REAL              :: spacing
            INTEGER           :: i

            IF (this%nvec .eq. 0) RETURN
            IF (this%nvec .eq. 1) THEN
               this%vec(1) = lower
                RETURN
            ENDIF

            spacing = (upper-lower)/(this%nvec-1)
            DO i = 1,this%nvec
                this%vec(i) = lower + spacing*(i-1)
            ENDDO
        END SUBROUTINE vector_linear_spacing

        FUNCTION vector_cell_entered(this) RESULT(carr)
            !
            ! Takes the vector object and finds the half-distance
            ! between all elements... Produces a results vector
            ! "vdiff" that is nvec-1 in size
            !
            CLASS(vector)     :: this
            REAL              :: carr(this%nvec-1)
            carr = 0.5 * (this%vec(2:) + this%vec(1:this%nvec-1))
        END FUNCTION vector_cell_entered

        FUNCTION vector_difference(this) RESULT(vdiff)
            !
            ! Takes the vector object and finds the difference
            ! between all elements... Produces a results vector
            ! "vdiff" that is nvec-1 in size
            !
            CLASS(vector) :: this
            REAL          :: vdiff(this%nvec-1)
            vdiff = this%vec(2:) - this%vec(1:this%nvec-1)
        END FUNCTION vector_difference

END MODULE class_vector