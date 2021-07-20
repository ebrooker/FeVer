SUBMODULE (state_class) state_advect

CONTAINS

    MODULE SUBROUTINE advect(this, nl, nr, area, velocity)
        CLASS(State_t), INTENT(INOUT) :: this
        INTEGER(smInt), INTENT(IN   ) :: nl,nr
        REAL(wp),       INTENT(IN   ) :: area(:), velocity(:)
        INTEGER(smInt) :: i


        DO CONCURRENT (i=nl:nr)
            IF ( velocity(i) > ZERO ) THEN
                this%flux(i) = velocity(i) * area(1)*this%intrfc_l(i)
            ELSE IF ( velocity(i) < ZERO ) THEN
                this%flux(i) = velocity(i) * area(1)*this%intrfc_r(i)
            ELSE
                this%flux(i) = HALF * &
                        ( area(1)*this%intrfc_l(i) + area(1)*this%intrfc_r(i) ) * &
                        ( velocity(i-1)              + velocity(i)              )
            END IF
        END DO
        

    END SUBROUTINE advect

END SUBMODULE state_advect