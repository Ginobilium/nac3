@extern
def output_int32(x: int32):
    ...
@extern
def output_uint32(x: uint32):
    ...
@extern
def output_int64(x: int64):
    ...
@extern
def output_uint64(x: uint64):
    ...
@extern
def output_float64(x: float):
    ...


def run() -> int32:
    output_int32(int32(min(True, False)))
    output_int32(int32(min(True, True)))
    output_int32(int32(min(False, False)))

    output_int32(min(-12, 0))
    output_int32(min(1, -3))
    output_int32(min(2, 2))
    
    output_int64(min(int64(-12), int64(0)))
    output_int64(min(int64(1), int64(-3)))
    output_int64(min(int64(2), int64(2)))
    
    output_uint32(min(uint32(12), uint32(0)))
    output_uint32(min(uint32(12), uint32(24)))
    output_uint32(min(uint32(2), uint32(2)))
    
    output_uint64(min(uint64(12), uint64(0)))
    output_uint64(min(uint64(12), uint64(24)))
    output_uint64(min(uint64(2), uint64(2)))

    output_float64(min(-12.234, 3.23))
    output_float64(min(0.1, 12.3))
    output_float64(min(1.1, 1.1))
    return 0
