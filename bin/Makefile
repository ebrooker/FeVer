

F90 = gfortran
FFLAGS = -ffree-line-length-200 -c

# module directories
IO = module_IO

OBJS =  globals.o \
		fdict.o \
		get_parameter.o \
		get_parameterInterface.o \
		grid.o \
		io_newUnit.o \
		io_parameterFile.o \
		
		


#
# make rules
#

fever: $(OBJS)
	$(F90) -o fever $(OBJS) fever.f03

fdict.mod: fdict.o fdict.f03
	$(F90) $(FFLAGS) fdict.f03

fdict.o: fdict.f03
	$(F90) $(FFLAGS) fdict.f03

globals.mod: globals.o globals.f03
	$(F90) $(FFLAGS) globals.f03

globals.o: globals.f03
	$(F90) $(FFLAGS) globals.f03

get_parameter.o: get_parameter.f03
	$(F90) $(FFLAGS) get_parameter.f03

get_parameterInterface.mod: get_parameterInterface.o get_parameterInterface.f03
	$(F90) $(FFLAGS) get_parameterInterface.f03

get_parameterInterface.o: get_parameterInterface.f03
	$(F90) $(FFLAGS) get_parameterInterface.f03

grid.o: grid.f03
	$(F90) $(FFLAGS) grid.f03	

io_newUnit.o: io_newUnit.f03
	$(F90) $(FFLAGS) io_newUnit.f03

io_parameterFile.mod: io_parameterFile.o io_parameterFile.f03
	$(F90) $(FFLAGS) io_parameterFile.f03

io_parameterFile.o: io_parameterFile.f03
	$(F90) $(FFLAGS) io_parameterFile.f03





# %.o: %.f03
# 	$(F90) $(FFLAGS) $^


# cleaning

clean:
	rm -f *.o *.mod fever


# DEPENDENCIES

# fdict.o:                    fdict.f03
# get_parameter.o:            get_parameter.f03
# get_parameterInterface.o:   get_parameterInterface.f03
# global.o:                   globals.f03
# io_newUnit.o:               io_newUnit.f03

# fever.o:                    fever.f03                    globals.mod   io_parameterFile.mod

# io_parameterFile.o:         io_parameterFile.f03         globals.mod   get_parameterInterface.mod   fdict.mod