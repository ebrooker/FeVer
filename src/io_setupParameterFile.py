import sys
print("    [PYTHON] Running setupInputFile.py")
print("    [PYTHON] Checking for Python 3.x")
if sys.version_info[0] == 2:
    print("    [PYTHON] Warning, Python 2 is no longer supported. Please change to Python 3")
    with open('parameters.fever','w') as f:
        f.write(-1)
    sys.exit(0)
    print("    [PYTHON] Aborting simulation")

import os
print("    [PYTHON] Checking that \"{0}\" parameter file exists".format(sys.argv[1]))
if os.path.isfile(sys.argv[1]):

    print("    [PYTHON] Exists... Reading in file...")
    with open(sys.argv[1],'r') as f:
        lines = f.readlines()

    lines = [line.strip() for line in lines if not line.startswith("#")]
    lines = list(filter(None,lines))
    lines = [line.split('=') for line in lines]

    # create a dictionary of the input params, overwriting old definitions of the same
    # entry in the input params setup file
    ldict = {line[0].strip():line[1].strip() for line in lines}
    lkeys = list(ldict.keys())
    lvals = list(ldict.values())
    lkeys,lvals = (list(t) for t in zip(*sorted(zip(lkeys,lvals))))
    print("    [PYTHON] Saving setup parameters to parameters.fever in FeVer format")
    # saves to file the last instance of all variables in the setup params file
    with open('parameters.fever', 'w') as f:
        for i in range(len(lkeys)):
            f.write('{0} {1}\n'.format(lkeys[i],lvals[i].split()[0]))
    print("    [PYTHON] Done, returning to FeVer...")

else:
    print("    [PYTHON] Warning, \"{0}\" parameter file does not exist!".format(sys.argv[1]))
    print("    [PYTHON] Aborting simulation")
