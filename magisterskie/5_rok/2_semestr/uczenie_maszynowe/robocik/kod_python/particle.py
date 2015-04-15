from numpy import *
from numpy.random import rand, randn
import random
import numpy

from pylab import colorbar, plot, imshow
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.cm as cm
my_cmap=LinearSegmentedColormap.from_list('my_cmap', [(1,1,1), (0.9882, 0.5529, 0.3490), (0.4980,0,0)]) 
my_cmap=cm.OrRd

def plot_particles(particles):
    res = 30
    extent = (0,1,0,1)

    img = zeros((res, res))
    X = particles[:,0]
    Y = particles[:,1]
    W = particles[:,-1]
    for x, y, w in zip(X, Y, W):
        if extent[0] <= x <= extent[1] and extent[2] <= y <= extent[3]:
            xi = int(x*res/extent[1])
            yi = int(y*res/extent[3])
            img[yi,xi]+=w
    imshow(img, extent = extent, origin="lower", cmap=my_cmap, interpolation = "nearest")
    colorbar(cmap=my_cmap)

def normalize_weights(particles):
    W = particles[:,-1]
    sum_of_weights = sum(W)
    for i in range(0,len(W)):
        w = float(W[i])/float(sum_of_weights)
        W[i] = w
    particles[:,-1] = W
    pass

def resample_particles(particles, tracks = None):
    l = len(particles)
    ktore = numpy.random.choice(l, l, p = particles[:,-1])
    new_particles = particles[ktore, :]
    new_particles[:, -1] = 3
    return new_particles

def effective_sample_size(particles):
    N = particles.shape[0]
    effw = 1.0 / sum(particles[:,-1]**2)
    effw /= N
    return effw

def particle_filter(Arena, resampT = 0.9, N = 100000):
    move_err =  Arena.robot.move_error
    rot_err =   Arena.robot.rot_error
    
    walls = Arena.get_walls()

    for t, (x, y, alpha) in enumerate(Arena.robot.waypoints):

        move, amount = Arena.robot.moves[t]
        print move, x, y, alpha * 180 / 3.1415927

        if move == "INIT":			
            particles = hstack([rand(N, 3), ones((N,1)) / N])
            particles[:,2] = particles[:,2] * 2 * pi - pi

        elif move == "ROT":
            particles[:,2] += amount + random.gauss(0, amount * rot_err)			
            pass

        elif move == "GO":
            for i in range(0, len(particles[:,0])):
                er = random.gauss(0, amount * move_err)
                particles[i,0] += (amount + er)*sin(particles[i,2])
                particles[i,1] += (amount + er)*cos(particles[i,2])
            pass

        else:
            print "wrong move", move
            return

             
        pom = zeros(len(particles[:,0]))
        
        for i in range(0, len(walls)):
            sciana = walls[i].on_wall(particles[:,0], particles[:,1])
            for j in range(0, len(pom)):
                if sciana[j] == 0:
                    pom[j]=1
        					
        for i in range(0, len(pom)):
            if pom[i]==1:
                particles[i,-1]=0  
		
        sensor_out = Arena.robot.read_sensor(t)
        particles[:,-1] *= Arena.robot.sensor.cond_prob(particles[:,0], particles[:,1], particles[:,2], sensor_out)

        # normalize weights
		
        normalize_weights(particles)

        effN = effective_sample_size(particles)
        print "efffective sample", effN
		
        # resample particles

        if effN < resampT and t < len(Arena.robot.waypoints) - 1:
            print "resampling"
            particles = resample_particles(particles)
        
        xz=x
        yz=y
        az=alpha

    plot_particles(particles)

	
	
	
	
	
	
	
	
	