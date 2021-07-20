# project/
#	- CMakeLists.txt
#	- src/

cmake_minimum_required(VERSION 2.8)

project(FeVer)
enable_language(Fortran)

set(CMAKE_Fortran_COMPILER "/opt/software/compilers/gnu/9.1.0/bin/gfortran")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -fimplicit-none -Wall -Wno-tabs")
    set(debug "-g -Wall, -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${debug}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

# 
# COMPILE
#
file(GLOB_RECURSE sources ../src/*.f90)
add_executable(fever ${sources})