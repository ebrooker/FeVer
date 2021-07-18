# OO-FeVer

object-oriented finite volume solver.

## Description

OO-FeVer is an object-oriented, (modern) Fortran-based finite volume solver for advection-diffusion-reaction equations. In principle, FeVer will be designed with an OOP mindset for the purpose of solving simple hydrodynamics problems. In the end, the hope is to have a complete Eulerian hydrodynamics solver (Riemann-solver) that additionally supports contributions from source terms, such as radioactive decay, nuclear fusion, ad hoc sources, etc. This software will eventually be adapted to HPC use through openMP and MPI. Prior to heavy duty parallelization, the code will attempt to make use of built-in Fortran parallelization, such as `DO CONCURRENT` loops and co-arrays. This project also gives a perfect opportunity to use the Fortran submodule paradigm, which can easily facilitate the setting up unique simulations by writing a specific data initialization subroutine that can be compiled separately as a plug-in feature assuming the subroutine interface doesn't change in the process (it should ideally NOT change).

Version 0.1 is nothing more than the original school project code written for a numerical partial differential equations course. This older version of FeVer (in 1D) is adequately implemented, but could use a major rehaul, which is the point of this project!

## Getting Started

### Dependencies

* Fortran compilers supporting up to Fortran 2008/2018 standards.
* CMake

### Executing program

Old FeVer-1D ( `old_FeVer_1D/` )
* ./run.sh
* ./clean.sh

OOP FeVer-1D ( `src/` )
* TBD

## Authors

Ezra Brooker

## Version History

* 0.1 (Alpha stage)


## License

This project is licensed under the [NAME HERE] License - see the LICENSE.md file for details... as soon as I set this up...
