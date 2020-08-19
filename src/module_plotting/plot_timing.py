import numpy as np

import matplotlib.pyplot as plt

from IPython import embed

data = np.loadtxt('timing.dat')

plt.loglog(data[:,1],data[:,0])
plt.xlabel(r'$log(n)$')
plt.ylabel(r'$log(t)\ [\mathrm{sec}]$')
plt.savefig('timing.png',dpi=750)
plt.close('all')
