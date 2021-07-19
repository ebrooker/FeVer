SUBMODULE (simulation_class) simulation_advect

CONTAINS

    MODULE SUBROUTINE advect(nl,nr,area,velocity,intrfc_l,intrfc_r,flux)
        REAL(wp), INTENT(IN   ) :: area(:), velocity(:), intrfc_l(:), intrfc_r(:)
        REAL(wp), INTENT(INOUT) :: flux(:)
        INTEGER(smInt)          :: i


        DO CONCURRENT (i=nl:nr+1)
            IF ( velocity(i) > ZERO ) THEN
                flux(i) = velocity(i) * area(i-1)*intrfc_l(i)
            ELSE IF ( velocity(i) < ZERO ) THEN
                flux(i) = velocity(i) * area(i  )*intrfc_r(i)
            ELSE
                flux(i) = HALF * &
                        ( area(i-1)*intrfc_l(i) + area(i)*intrfc_r(i) ) * &
                        ( velocity(i-1)         + velocity(i)         )
            END IF
        END DO

    END SUBROUTINE advect

END SUBMODULE simulation_advect