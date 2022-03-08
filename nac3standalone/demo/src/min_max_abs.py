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
    # min ===========
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
    
    # max ===========
    output_int32(int32(max(True, False)))
    output_int32(int32(max(True, True)))
    output_int32(int32(max(False, False)))

    output_int32(max(-12, 0))
    output_int32(max(1, -3))
    output_int32(max(2, 2))
    
    output_int64(max(int64(-12), int64(0)))
    output_int64(max(int64(1), int64(-3)))
    output_int64(max(int64(2), int64(2)))
    
    output_uint32(max(uint32(12), uint32(0)))
    output_uint32(max(uint32(12), uint32(24)))
    output_uint32(max(uint32(2), uint32(2)))
    
    output_uint64(max(uint64(12), uint64(0)))
    output_uint64(max(uint64(12), uint64(24)))
    output_uint64(max(uint64(2), uint64(2)))

    output_float64(max(-12.234, 3.23))
    output_float64(max(0.1, 12.3))
    output_float64(max(1.1, 1.1))

    # abs ===========
    output_int32(int32(abs(False)))
    output_int32(int32(abs(True)))

    output_int32(abs(0))
    output_int32(abs(-3))
    output_int32(abs(2))
    
    output_int64(abs(int64(-12)))
    output_int64(abs(int64(0)))
    output_int64(abs(int64(2)))
    
    output_uint32(abs(uint32(0)))
    output_uint32(abs(uint32(24)))
    
    output_uint64(abs(uint64(0)))
    output_uint64(abs(uint64(24)))

    output_float64(abs(-12.234))
    output_float64(abs(-0.0))
    output_float64(abs(1.1))

    return 0
