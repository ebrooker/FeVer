SUBMODULE (state_class) state_reconstruction

CONTAINS

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


END SUBMODULE state_reconstruction