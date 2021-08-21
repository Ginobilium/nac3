def run() -> int32:
    minX = -2.0
    maxX = 1.0
    width = 78.0
    height = 36.0
    aspectRatio = 2.0

    test = 1.0 + 1

    yScale = (maxX-minX)*(height/width)*aspectRatio

    y = 0.0
    while y < height:
        x = 0.0
        while x < width:
            c_r = minX+x*(maxX-minX)/width
            c_i = y*yScale/height-yScale/2.0
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
            x = x + 1.0
        output(-1)
        y = y + 1.0
    return 0

