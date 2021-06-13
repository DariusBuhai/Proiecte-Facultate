from util import *
from ex1 import *


def graham_scan_inferior(p):
    l = [p[0], p[1]]
    for i in range(2, len(p)):
        l.append(p[i])
        while len(l) > 2 and compute_turn(l[-3], l[-2], l[-1]) > 0:
            l.pop(-2)
    return l


def graham_scan_superior(p):
    l = [p[0], p[1]]
    for i in range(2, len(p)):
        l.append(p[i])
        while len(l) > 2 and compute_turn(l[-3], l[-2], l[-1]) <= 0:
            l.pop(-2)
    return l

if __name__ == '__main__':
    points = read_points()
    # points = [(-8, 0), (-6, -2), (-4, 0), (-4, 2), (-3, 2), (-2, 2), (0, -1), (2, -4)]

    points.sort(key=lambda point: point[1])
    points.sort(key=lambda point: point[0])
    inferior = graham_scan_inferior(points)

    points.sort(key=lambda point: point[0])
    points.sort(key=lambda point: point[1])
    superior = graham_scan_superior(points)

    inferior.extend(superior)
    convex_points = inferior

    print(convex_points)
