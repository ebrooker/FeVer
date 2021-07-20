#!/bin/bash

FEVER_DIR=../
cd $FEVER_DIR

if [ -d "$FEVER_DIR/build" ]
then
	echo "Build directory already exists, deleting it now..."
	rm -r build/
fi

echo "Creating build directory for CMake..."
mkdir build

echo "Entering the build directory..."
cd build

echo "Building FeVer project with CMake in BUILD MODE..."
cmake $FEVER_DIR/bin/

echo "Compiling FeVer project with make..."
make

if [ -f "$FEVER_DIR/build/fever" ]
then
	echo "FeVer successfully built..."
	echo "Executing code..."
	./fever
	echo ""
	echo "Have a nice day"
	echo ""
else
	echo "FeVer failed to build..."
	echo "What went wrong?"
	echo ""
fi