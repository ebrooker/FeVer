# OO-FeVer

object-oriented finite volume solver.

## Description

OO-FeVer is an object-oriented, (modern) Fortran-based finite volume solver for advection-diffusion-reaction equations. In principle, FeVer will be designed with an OOP mindset for the purpose of solving simple hydrodynamics problems. In the end, the hope is to have a complete hydrodynamics solver (Riemann-solver) that additionally supports contributions from source terms, such as radioactive decay, nuclear fusion, ad hoc sources, etc. This software will eventually be adapted to HPC use through openMP and MPI. Prior to heavy duty parallelization, the code will attempt to make use built-in Fortran parallelization, such as `DO CONCURRENT` loops and co-arrays.

## Getting Started

### Dependencies

* Fortran compilers supporting up to Fortran 2008/2018 standards.
* CMake

### Installing

* TBD
* TBD

### Executing program

* TBD
* TBD


## Authors

Ezra Brooker

## Version History

* 0.1


## License

This project is licensed under the [NAME HERE] License - see the LICENSE.md file for details... as soon as I set this up...
