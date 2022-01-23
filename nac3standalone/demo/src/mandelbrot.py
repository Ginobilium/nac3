@extern
def output_asciiart(x: int32):
    ...


def run() -> int32:
    minX = -2.0
    maxX = 1.0
    width = 78
    height = 36
    aspectRatio = 2.0

    yScale = (maxX-minX)*(float(height)/float(width))*aspectRatio

    y = 0
    while y < height:
        x = 0
        while x < width:
            c_r = minX+float(x)*(maxX-minX)/float(width)
            c_i = float(y)*yScale/float(height)-yScale/2.0
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
            output_asciiart(i)
            x = x + 1
        output_asciiart(-1)
        y = y + 1
    return 0
