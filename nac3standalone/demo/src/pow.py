@extern
def output_float64(f: float):
    ...


def run() -> int32:
    output_float64(float(3 ** 1))
    output_float64(float(3 ** 0))
    output_float64(float(3 ** 19))
    output_float64(1.0 ** -100)
    output_float64(1.0 ** -2)
    output_float64(1.0 ** 0)
    output_float64(1.0 ** 1)
    output_float64(1.0 ** 100)
    output_float64(3.0 ** 0)
    output_float64(3.0 ** 1)
    output_float64(3.0 ** 2)
    output_float64(3.0 ** -1)
    output_float64(3.0 ** -2)
    output_float64(3.0 ** -32767)
    output_float64(3.0 ** -3.0)
    output_float64(3.0 ** -0.0)
    output_float64(3.0 ** 0.0)
    output_float64(4.0 ** 0.5)
    output_float64(4.0 ** -0.5)
    return 0