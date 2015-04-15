# przyklad 1

import random
import numpy
from numpy import pi
from pylab import show, draw, ion

from arena import Wall, Robot, Arena
from particle import *

if __name__ == "__main__":
    
   # random.seed(10)
   # numpy.random.seed(10)

    w11 = Wall(0.15, 0.05, 0.15, 0.45)
    w12 = Wall(0.15, 0.55, 0.15, 0.95)
    w2 = Wall(0.15, 0.95, 0.7, 0.95)
    w3 = Wall(0.2, 0.5, 0.7, 0.5)

    r = Robot(0.9, 0.1, 0)

    for i in xrange(3):
        r.move("GO", 0.06)
    
    r.move("ROT", -pi/2)
    r.move("GO", 0.06)
    r.move("ROT", pi/6)
    
    for i in xrange(4):
        r.move("GO", 0.06)
        r.move("ROT", pi/2)
        r.move("ROT", -pi/2)
    
    for i in xrange(8):
        r.move("ROT", -pi/8)
    
    r.move("GO", 0.4)
    r.move("ROT", -pi/2)
    
    for i in xrange(5):
        r.move("GO", 0.06) 	
    
    r.move("ROT", -pi/4)
    
    for i in xrange(6):
        r.move("GO", 0.06)
    
    r.move("ROT", pi/2)
    
    Arena.plot()
    
    #particle_filter(Arena, N = 100000, resampT = -1)
    particle_filter(Arena, N = 100000)
    
    show()
	
	