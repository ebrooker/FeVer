PROGRAM main
USE kind_settings
USE grid_class, only : Grid_t
USE simulation_class, only : Simulation_t
IMPLICIT NONE

    TYPE(Simulation_t) :: simulation

    simulation = Simulation_t("fever.inp")
    print*, simulation%inputfile


    CALL simulation%evolve()


END PROGRAM main