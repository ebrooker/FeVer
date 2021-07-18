#
#
#MAKEFILE for Ezra S Brooker's Finite Volume Linear Advection code
#
#

F90 = gfortran

FLAGS = -ffree-line-length-200 -c

OBJS = globals.o \
	linear_solvers.o \
	tricyclic.o \
	tests.o \
	init_simulation_interface.o \
	init_parameters_get.o \
	init_data_from_file.o \
	init_parameters_read.o \
	init_data_mesh.o \
	init_data_velocity.o \
	init_data_states.o \
	init_simulation_driver.o \
	boundary_conditions.o \
    advection_fluxes.o \
    diffusion_fluxes.o \
    diffusion_implicit.o \
    diagnostics.o \
    evolve.o \
    output.o \
    reconstruct_hydrostate.o \
    source_terms.o \
    timestep.o \
    update.o \

#
# make rules
#


fever:	$(OBJS)
	$(F90) -o fever $(OBJS) fever.f90

globals.mod: globals.o globals.f90
	$(F90) $(FLAGS) globals.f90

globals.o: globals.f90
	$(F90) $(FLAGS) globals.f90

linear_solvers.mod: linear_solvers.o linear_solvers.f90
	$(F90) $(FLAGS) linear_solvers.f90

linear_solvers.o: linear_solvers.f90
	$(F90) $(FLAGS) linear_solvers.f90

tricyclic.o: tricyclic.f
	$(F90) $(FLAGS) tricyclic.f

tests.mod: tests.o tests.f90
	$(F90) $(FLAGS) tests.f90

tests.o: tests.f90
	$(F90) $(FLAGS) tests.f90

init_simulation_interface.mod: init_simulation_interface.o init_simulation_interface.f90
	$(F90) $(FLAGS) init_simulation_interface.f90

init_simulation_interface.o: init_simulation_interface.f90
	$(F90) $(FLAGS) init_simulation_interface.f90

init_parameters_get.o: init_parameters_get.f90
	$(F90) $(FLAGS) init_parameters_get.f90

init_data_from_file.o: init_data_from_file.f90
	$(F90) $(FLAGS) init_data_from_file.f90

init_parameters_read.o: init_parameters_read.f90
	$(F90) $(FLAGS) init_parameters_read.f90

init_data_mesh.o: init_data_mesh.f90
	$(F90) $(FLAGS) init_data_mesh.f90

init_data_velocity.o: init_data_velocity.f90
	$(F90) $(FLAGS) init_data_velocity.f90

init_data_states.o: init_data_states.f90
	$(F90) $(FLAGS) init_data_states.f90

init_simulation_driver.o: init_simulation_driver.f90
	$(F90) $(FLAGS) init_simulation_driver.f90

boundary_conditions.o: boundary_conditions.f90
	$(F90) $(FLAGS) boundary_conditions.f90

advection_fluxes.o: advection_fluxes.f90
	$(F90) $(FLAGS) advection_fluxes.f90

diffusion_fluxes.o: diffusion_fluxes.f90
	$(F90) $(FLAGS) diffusion_fluxes.f90

diffusion_implicit.o: diffusion_implicit.f90
	$(F90) $(FLAGS) diffusion_implicit.f90

diagnostics.o: diagnostics.f90
	$(F90) $(FLAGS) diagnostics.f90

evolve.o: evolve.f90
	$(F90) $(FLAGS) evolve.f90

output.o: output.f90
	$(F90) $(FLAGS) output.f90

reconstruct_hydrostate.o: reconstruct_hydrostate.f90
	$(F90) $(FLAGS) reconstruct_hydrostate.f90

source_terms.o: source_terms.f90
	$(F90) $(FLAGS) source_terms.f90

timestep.o: timestep.f90
	$(F90) $(FLAGS) timestep.f90

update.o: update.f90
	$(F90) $(FLAGS) update.f90


clean:
	rm -f *.o *.mod fever 


# DEPENDENCIES

linear_solvers.o:            linear_solvers.f90

init_simulation_interface.o: init_simulation_interface.f90

init_parameters_get.o:       init_parameters_get.f90         globals.mod
init_data_from_file.o:       init_data_from_file.f90         globals.mod

init_parameters_read.o:      init_parameters_read.f90        globals.mod   init_simulation_interface.mod
init_data_mesh.o:            init_data_mesh.f90              globals.mod   init_simulation_interface.mod
init_data_velocity.o:        init_data_velocity.f90          globals.mod   init_simulation_interface.mod
init_data_states.o:          init_data_states.f90            globals.mod   init_simulation_interface.mod
init_simulation_driver.o:    init_simulation_driver.f90      globals.mod   tests.mod

boundary_conditions.o:       boundary_conditions.f90         globals.mod
advection_fluxes.o:          advection_fluxes.f90            globals.mod
diffusion_fluxes.o:          diffusion_fluxes.f90            globals.mod
diffusion_implicit.o:        diffusion_implicit.f90          globals.mod   linear_solvers.mod
diagnostics.o:               diagnostics.f90                 globals.mod
evolve.o:                    evolve.f90                      globals.mod
output.o:                    output.f90                      globals.mod
reconstruct_hydrostate.o:    reconstruct_hydrostate.f90      globals.mod
source_terms.o:              source_terms.f90                globals.mod
timestep.o:                  timestep.f90                    globals.mod
update_hydrostate.o:         update_hydrostate.f90           globals.mod
tests.o:                     tests.f90                       globals.mod
fever.o:                     fever.f90                       globals.mod
 
tricyclic.o: tricyclic.f
