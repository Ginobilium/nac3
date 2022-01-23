@extern
def output_int32_list(x: list[int32]):
    ...

def run() -> int32:
    data = [0, 1, 2, 3]

    output_int32_list(data[2:3])
    output_int32_list(data[:])
    output_int32_list(data[1:])
    output_int32_list(data[:-1])

    m1 = -1
    output_int32_list(data[::m1])
    output_int32_list(data[:0:m1])

    m2 = -2
    output_int32_list(data[m2::m1])

    return 0
