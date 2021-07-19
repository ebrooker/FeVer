#!/bin/bash

rm fever.exe

gfortran -c kind_settings.f90
gfortran -c global_constants.f90
gfortran -c io_newUnit.f90
gfortran -c entry_class.f90
gfortran -c dictionary_class.f90 
gfortran -c state_class.f90 
gfortran -c grid_class.f90

gfortran -c simulation_class.f90 
gfortran -c simulation_advect.f90
gfortran -c simulation_boundaryConditions.f90
gfortran -c simulation_data.f90
gfortran -c simulation_evolve.f90
gfortran -c simulation_get.f90
gfortran -c simulation_grid.f90
gfortran -c simulation_init.f90
gfortran -c simulation_put.f90
gfortran -c simulation_readInput.f90
gfortran -c simulation_reconstruction.f90
gfortran -c simulation_states.f90
gfortran -c simulation_timestep.f90

gfortran *.o fever.f90 -o fever.exe

./fever.exe

rm *.o *.mod *.smod
