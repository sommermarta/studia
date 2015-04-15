import random
import numpy
from numpy import pi

from pylab import show, draw, ion

from arena import Wall, Robot, Arena
from particle import *

if __name__ == "__main__":
    random.seed(10)
    numpy.random.seed(10)

    ### WORLD 1
    w11 = Wall(0.15, 0.05, 0.15, 0.45)
    w12 = Wall(0.15, 0.55, 0.15, 0.95)
    w2 = Wall(0.15, 0.95, 0.7, 0.95)
    w3 = Wall(0.2, 0.5, 0.7, 0.5)

    # a sequence of moves
    r = Robot(0.95, 0.05, 0)
    #for i in xrange(4):
    #    r.move("GO", 0.06);
    #Arena.plot()
    #particle_filter(Arena, N = 100, resampT = -1)

    # interactive control
    ion()
    Arena.plot()
    particle_filter(Arena)
    while True:
        cmd = raw_input()
        if cmd == "q":
            break
        elif cmd == "l":
            r.move("ROT", -pi / 8)
        elif cmd == "r":
            r.move("ROT", pi / 8)
        elif cmd == "f":
            r.move("GO", 0.1)
        elif cmd == "b":
            r.move("GO", -0.1)
        Arena.plot()
        particle_filter(Arena)
        draw()

		
    show()
