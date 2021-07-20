import sys,glob
import numpy as np
import matplotlib.pyplot as plt

from IPython import embed

files = glob.glob(f"../build/fort.*")

nums = [int(f.split("fort.")[1]) for f in files]
nums.sort()

files = [f"../build/fort.{n}" for n in nums]

yi = np.genfromtxt(files[0])
yf = np.genfromtxt(files[-1])


print(yi)
print(yf)
x = np.linspace(0,1,yi.size)
plt.plot(x,yi,label='Initial')
plt.plot(x,yf,label='Final')

plt.show()