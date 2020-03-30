from numpy import int32, float64
import sys


def run() -> int32:
    minX = -2.0
    maxX = 1.0
    width = 78
    height = 36
    aspectRatio = 2.0

    yScale = float64(maxX-minX)*(float64(height)/float64(width))*aspectRatio

    y = 0
    while y < height:
        x = 0
        while x < width:
            c_r = minX+float64(x)*(maxX-minX)/float64(width)
            c_i = float64(y)*yScale/float64(height)-yScale/2.0
            z_r = c_r
            z_i = c_i
            i = 0
            while i < 16:
                if z_r*z_r + z_i*z_i > 4.0:
                    break
                new_z_r = (z_r*z_r)-(z_i*z_i) + c_r
                z_i = 2.0*z_r*z_i + c_i
                z_r = new_z_r
                i = i + 1
            output(i)
            x = x + 1
        output(-1)
        y = y + 1
    return 0



def output(i):
    if i >= 0:
        sys.stdout.write(" .,-:;i+hHM$*#@  "[i])
    else:
        print("")

run()
