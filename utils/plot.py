import sys,glob
import numpy as np
import matplotlib.pyplot as plt

yi = np.loadtxt("data/old.csv", skiprows=1, delimiter=",")
yf = np.loadtxt("data/new.csv", skiprows=1, delimiter=",")

plt.plot(yi[:,0],yi[:,1],label='Old')
plt.plot(yf[:,0],yf[:,1],label='New', linestyle='--')

plt.legend()
plt.show()