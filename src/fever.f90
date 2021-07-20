PROGRAM main
USE kind_settings
USE grid_class, only : Grid_t
USE simulation_class, only : Simulation_t
IMPLICIT NONE

    TYPE(Simulation_t) :: simulation

    simulation = Simulation_t("/data2/eb11d/misc/my_libs/github/FeVer/src/fever.inp")
    print*, simulation%inputfile

    CALL simulation%init("/data2/eb11d/misc/my_libs/github/FeVer/src/fever.inp")

    CALL simulation%evolve()


END PROGRAM main