#!/bin/bash

rm fever.exe

gfortran -c kind_settings.f90 global_constants.f90 io_newUnit.f90 \
            entry_class.f90 dictionary_class.f90 

gfortran -c state_class.f90 

gfortran -c grid_class.f90

gfortran -c simulation_class.f90 simulation_init.f90 simulation_data.f90 simulation_grid.f90 \
            simulation_states.f90 simulation_get.f90 simulation_put.f90 simulation_readInput.f90 \
            simulation_evolve.f90 simulation_timestep.f90 simulation_boundaryConditions.f90 \
            simulation_reconstruction.f90 simulation_advectiveFlux.f90

gfortran *.o fever.f90 -o fever.exe

./fever.exe

rm *.o *.mod *.smod
