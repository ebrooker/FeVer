import sys,glob
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

from matplotlib.animation import FuncAnimation, PillowWriter

class Animator:

    def __init__(self, files):
        self.fig, self.ax = plt.subplots()
        self.files = files
        self.__plot(0)
        
        self.ax.set_xlabel("x [unitless]")
        self.ax.set_ylabel("u [unitless]")

    def __plot(self, i):
        data = np.loadtxt(self.files[i], skiprows=1, delimiter=",")
        self.line, = self.ax.plot(data[:,0], data[:,2])
        mmin, mmax = data[:,2].min(), data[:,2].max()
        self.ax.set_ylim(mmin - 0.1*mmin, mmax + 0.1*mmax)

    def __update(self, i):
        data = np.loadtxt(self.files[i], skiprows=1, delimiter=",")
        self.line.set_xdata(data[:,0])
        self.line.set_ydata(data[:,2])
        self.ax.set_title(f"Frame {i}")
        return self.line,

    def animate(self):
        anim = FuncAnimation(self.fig, self.__update, frames=len(self.files), interval=100)
        anim.save("movies/fever_example.gif", writer=PillowWriter(fps=30))

cwd = Path.cwd()
data_dir = cwd / "data"
data_files = sorted(list(data_dir.glob("fever_snp_*")))
movie_dir = cwd / "movies"
movie_dir.mkdir(exist_ok=True)

animator = Animator(data_files)
animator.animate()
