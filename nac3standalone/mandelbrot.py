def y_scale(maxX: float, minX: float, height: float, width: float, aspectRatio: float) -> float:
    return (maxX-minX)*(height/width)*aspectRatio

def check_smaller_than_sixteen(i: int32) -> bool:
    return i < 16

def rec(x: int32):
    if x > 1:
        output(x)
        rec(x - 1)
        return
    else:
        output(-1)
        return

def fib(n: int32) -> int32:
    if n <= 2:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)

def draw():
    minX = -2.0
    maxX = 1.0
    width = 78.0
    height = 36.0
    aspectRatio = 2.0

    # test = 1.0 + 1

    yScale = y_scale(maxX, minX, height, width, aspectRatio)

    y = 0.0
    while y < height:
        x = 0.0
        while x < width:
            c_r = minX+x*(maxX-minX)/width
            c_i = y*yScale/height-yScale/2.0
            z_r = c_r
            z_i = c_i
            i = 0
            while check_smaller_than_sixteen(i):
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
    
    return

def run() -> int32:
    rec(5)

    output(fib(10))
    output(-1)
    
    draw()
    return 0

