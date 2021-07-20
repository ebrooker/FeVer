
FEVER_DIR=/data2/eb11d/misc/my_libs/github/FeVer/

cd $FEVER_DIR/build

if [ -f "$FEVER_DIR/build/fever" ]
then
	echo "FeVer successfully built..."
	echo "Executing code..."
	./numlib
	echo ""
	echo "Have a nice day"
	echo ""
else
	echo "FeVer failed to build..."
	echo "What went wrong?"
	echo ""
fi