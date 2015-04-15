import random
import numpy
from numpy import pi
from pylab import show, draw, ion

from arena import Wall, Robot, Arena
from importance import *

if __name__ == "__main__":

    random.seed(10)
    numpy.random.seed(10)

    w11 = Wall(0.15, 0.05, 0.15, 0.45)
    w12 = Wall(0.15, 0.55, 0.15, 0.95)
    w2 = Wall(0.15, 0.95, 0.7, 0.95)
    w3 = Wall(0.2, 0.5, 0.7, 0.5)

    r = Robot(0.95, 0.05, 0)
    
    Arena.plot()
    
    for i in xrange(4):
        r.move("GO", 0.06);
    Arena.plot()
    # particle_filter(Arena, N = 100000, resampT = -1)
    particle_filter(Arena, N = 100000)
    show()
	
	
	