@extern
def output_int32_list(x: list[int32]):
    ...

@extern
def output_int32(x: int32):
    ...

class A:
    a: int32
    b: bool
    def __init__(self, a: int32, b: bool):
        self.a = a
        self.b = b

def run() -> int32:
    data = [0, 1, 2, 3]

    t = [(d, d + d) for d in data]
    for i in t:
        tt = (Some(i[1]), i[0])
        tl = ([i[0], i[1] + i[0]], i[1])
        output_int32(tt[0].unwrap())
        output_int32(tt[1])
        output_int32(tl[0][0])
        output_int32(tl[0][1])
        output_int32(tl[1])
    
    return 0