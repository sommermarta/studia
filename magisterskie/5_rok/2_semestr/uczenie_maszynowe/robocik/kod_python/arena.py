from math import sqrt, sin, cos
from random import gauss

from numpy import zeros_like

from pylab import *
from matplotlib.lines import Line2D
from matplotlib.patches import Circle


def dist(x0, y0, x1, y1):
    return sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0))
def sqdist(x0, y0, x1, y1):
    return (x1-x0)*(x1-x0) + (y1-y0)*(y1-y0)

class Arena(object):
    arena = None
    objects = []
    robot = None
    @staticmethod
    def plot():
        clf()
        gca().set_aspect("equal")
        for o in Arena.objects:
            o.plot()
        if Arena.robot is not None:
            Arena.robot.plot()
    @staticmethod
    def get_walls():
        walls = []
        for o in Arena.objects:
            if isinstance(o, Wall):
                walls.append(o)
        return walls



class Wall(object):
    def __init__(self, x0, y0, x1, y1):
        self.x0 = x0
        self.y0 = y0
        self.x1 = x1
        self.y1 = y1
        self.dx = x1 - x0
        self.dy = y1 - y0
        self.det = self.dx * self.dx + self.dy * self.dy
        Arena.objects.append(self)
    def plot(self):
        gca().add_line(Line2D([self.x0, self.x1], [self.y0, self.y1], linewidth=3))
    def dist_point_wall(self, x, y):
        """Squared distance of (x,y) point from the wall."""
        # is point in front of the wall?
        # find projection of point on wall
        t0 = (self.dx * (x - self.x0) + self.dy * (y - self.y0)) / self.det
        d = zeros_like(x)
        mask = (0 <= t0) & (t0 <= 1)
        # we're facing the wall
        xw = self.x0 + t0[mask] * self.dx
        yw = self.y0 + t0[mask] * self.dy
        xm = x[mask]
        ym = y[mask]
        d[mask] = sqdist(xw, yw, xm, ym)
        # how close are we to edges?
        xnm = x[~mask]
        ynm = y[~mask]
        d[~mask] = vstack([sqdist(xnm, ynm, self.x0, self.y0),
                           sqdist(xnm, ynm, self.x1, self.y1)
                           ]).min(axis = 0)
        return d
    def ray_wall(self, x, y, alpha):
        """Interaction between the wall and a ray cast from (x,y) at
        angle alpha."""
        sa = -sin(alpha)
        ca = cos(alpha)
        det = -self.dx * ca - self.dy * sa
        if isscalar(det):
            if det == 0:
                det = 1e-10
        else:
            det[det==0] = 1e-10
        t0 = (-ca * (x - self.x0) - sa * (y - self.y0)) / det
        t1 = (self.dx * (y - self.y0) - self.dy * (x - self.x0)) / det
        mask = (0 <= t0) & (t0 <= 1) & (t1 > 0)
        if len(mask.shape) > 0:
            xw = self.x0 + t0[mask] * self.dx
            yw = self.y0 + t0[mask] * self.dy
            xm = x[mask]
            ym = y[mask]
            d = empty_like(x)
            d[mask] = sqdist(xw, yw, xm, ym)
            d[~mask] = 10e20
        else:
            if mask:
                xw = self.x0 + t0 * self.dx
                yw = self.y0 + t0 * self.dy
                d = sqdist(xw, yw, x, y)
            else:
                d = 10e20
        return d
    def on_wall(self, x, y):
        d = self.dist_point_wall(x, y)
        r = Arena.robot.r
        return 1.0 * (d > r * r)

def gauss_pdf(x, mu, sigma = 1):
    return 1.0/(sigma * sqrt(2.0*pi)) * exp(-(x-mu)*(x-mu)/(2*sigma*sigma))
def log_gauss_pdf(x, mu, sigma = 1):
    return -log(sigma * sqrt(2.0*pi)) -(x-mu)*(x-mu)/(2*sigma*sigma)

class Sensor(object):
    noise = 0.1
    last_arg = None
    last_expected = None
    @staticmethod
    def expected(x, y, alpha):
        """Expected sensor readout when sensor is located at (x,y) in
        direction alpha."""
        dists = [Wall.ray_wall(x, y, alpha) for Wall in Arena.get_walls()]
        d = vstack(dists).min(axis = 0)
        d *= 100
        pwr = 1.0 / d
        return pwr
    @staticmethod
    def plot(x, y, alpha, col = "k", blend = 1):
        ax = gca()
        ran = 0.1
        x1, y1 = x + sin(alpha)*ran, y + cos(alpha)*ran
        ax.add_line(Line2D((x, x1), (y, y1), color=col, alpha = blend))
    @staticmethod
    def cond_prob(x, y, alpha, sensor_out):
        expected_pwr = Sensor.expected(x, y, alpha)
        return gauss_pdf(sensor_out, expected_pwr, Sensor.noise)
    @staticmethod
    def log_cond_prob(x, y, alpha, sensor_out):
        expected_pwr = Sensor.expected(x, y, alpha)
        return log_gauss_pdf(sensor_out, expected_pwr, Sensor.noise)
        

class Robot(object):
    def __init__(self, x, y, alpha):
        self.x = x
        self.y = y
        self.alpha = alpha
        self.waypoints = [[x, y, alpha]]
        self.moves = [("INIT", 0)]
        self.r = 0.05
        self.rot_error = 0.0
        self.move_error = 0.0
        self.rot_error = 0.1
        self.move_error = 0.1
        self.sensor = Sensor
        Arena.robot = self
    def move(self, move, amount):
        if move == "ROT":
            self.alpha += amount + gauss(0, amount * self.rot_error)
            while self.alpha > pi:
                self.alpha -= 2*pi
            while self.alpha < -pi:
                self.alpha += 2*pi
        elif move == "GO":
            amount += gauss(0, amount * self.move_error)
            self.x += amount * sin(self.alpha)
            self.y += amount * cos(self.alpha)
        else:
            raise RuntimeError("Bad move")
        self.waypoints.append([self.x, self.y, self.alpha])
        self.moves. append((move, amount))
    def read_sensor(self, t = -1):
        x, y, alpha = self.waypoints[t]
        return Sensor.expected(x, y, alpha)# + gauss(0, Sensor.noise)
    def plot(self):
        ax = gca()
        x1 = None; y1 = None
        for x, y, alpha in self.waypoints[:-1]:
            if x1 is not None:
                ax.add_line(Line2D((x, x1), (y, y1), color="k", alpha = 1))
            ax.add_patch(Circle((x, y), self.r, fill=False, edgecolor="b", alpha=0.25))
            Sensor.plot(x, y, alpha, col = "b", blend = 0.25)
            x1 = x; y1 = y
        ax.add_line(Line2D((self.x, x1), (self.y, y1), color="k", alpha = 1))
        ax.add_patch(Circle((self.x, self.y), self.r, fill=False, linewidth = 1))
        Sensor.plot(self.x, self.y, self.alpha, col = "b", blend = 0.25)
