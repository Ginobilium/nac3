@extern
def output_int32(x: int32):
    ...

def run() -> int32:
    for _ in range(10):
        output_int32(_)
        _ = 0
    return 0
