@extern
def output_int32(x: int32):
    ...


def run() -> int32:
    for l in [[1, 2, 3, 4, 5], [1, 2, 3], [1], []]:
        output_int32(len(l))

    for r in [
            range(10),
            range(0, 0),
            range(2, 10),
            range(0, 10, 1),
            range(1, 10, 2),
            range(-1, 10, 2),
            range(2, 10, 3),
            range(-2, 12, 5),
            range(2, 5, -1),
            range(5, 2, -1),
            range(5, 2, -2),
            range(24, -3, -6),
            range(24, -3, 6)]:
        output_int32(len(r))
    return 0
