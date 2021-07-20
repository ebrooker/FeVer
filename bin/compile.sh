
mkdir ../testdir
cd ../testdir

cp ../src/Utils/kindSettings.f90 ./
cp ../src/Utils/multiVarFuncIntrfc.f90 ./

cp ../src/LinearAlgebra/linearAlgebra.f90 ./

cp ../src/Functions/funcs.f90 ./
cp ../src/Functions/derivatives.f90 ./
cp ../src/Functions/functionInterface.f90 ./

cp ../src/Optimization/GlobalMinimization/lineSearch.f90 ./
cp ../src/Optimization/optimization.f90 ./

cp ../src/Differentiation/RichardsonExtrapolation/richardsonExtrapolation.f90 ./
cp ../src/Differentiation/FiniteDifferences/finiteDifferences.f90 ./
cp ../src/Differentiation/FiniteDifferences/centralDifferences.f90 ./
cp ../src/Differentiation/FiniteDifferences/forwardDifferences.f90 ./
cp ../src/Differentiation/FiniteDifferences/backwardDifferences.f90 ./
cp ../src/Differentiation/differentiation.f90 ./

# cp ../src/Test/proctorClass.f90 ./
cp ../src/Test/proctorOptimize.f90 ./
cp ../src/Test/proctorDerivative.f90 ./
cp ../src/Test/test.f90 ./

gfortran -c kindSettings.f90
gfortran -c multiVarFuncIntrfc.f90
gfortran -c funcs.f90 derivatives.f90 functionInterface.f90

gfortran -c linearAlgebra.f90

gfortran -c lineSearch.f90 optimization.f90


gfortran -c richardsonExtrapolation.f90 centralDifferences.f90 \
            forwardDifferences.f90 backwardDifferences.f90 \
            finiteDifferences.f90
gfortran -c differentiation.f90

# gfortran -c proctorClass.f90
gfortran -c proctorOptimize.f90
gfortran -c proctorDerivative.f90


gfortran *.o test.f90 -o numlib
./numlib
rm numlib *.mod *.o
