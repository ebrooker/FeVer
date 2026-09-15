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
        _, = self.ax.plot(data[:,0], data[:,2])
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
        return anim



if __name__ == "__main__":
    cwd = Path.cwd()
    data_dir = cwd / "data"
    movie_dir = cwd / "movies"
    movie_dir.mkdir(exist_ok=True)

    examples = ["advection", "burgers"]
    examples = [ "burgers"]

    for example in examples:
        data_files = sorted(list(data_dir.glob(f"{example}_snp_*")))

        animator = Animator(data_files)
        animator.animate().save(f"movies/{example}.gif", writer=PillowWriter(fps=30))

        plt.close('all')
        init  = np.loadtxt(data_files[0], skiprows=1, delimiter=",")
        final  = np.loadtxt(data_files[-1], skiprows=1, delimiter=",")
        plt.plot(init[:,0], init[:,1], label="Initial")
        plt.plot(final[:,0], final[:,1], label="Final")
        plt.xlabel("x [cm]")
        plt.ylabel("y [variable]")
        plt.legend()
        plt.tight_layout()
        plt.savefig(f"plots/{example}.png", dpi=1024)