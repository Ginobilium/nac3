@extern
def output_int32(x: int32):
    ...

def f1(a: int32 = 4):
    output_int32(a)

def f2(a: int64 = int64(123)):
    output_int32(int32(a))

def f3(a: uint32 = uint32(234)):
    output_int32(int32(a))

def f4(a: tuple[int32, tuple[int32, int32], int64] = (4, (5, 6), int64(7))):
    output_int32(a[0])
    output_int32(a[1][0])
    output_int32(a[1][1])
    output_int32(int32(a[2]))

def f5(a: float = 3.45):
    output_int32(int32(a))

def f6(a: Option[list[int32]] = none):
    if a.is_none():
        a = Some([11,22,33])
        output_int32(a.unwrap()[2])

def f7(a: Option[tuple[int32, int64]] = Some((3, int64(123)))):
    if a.is_some():
        a_unwrap = a.unwrap()
        output_int32(a_unwrap[0])
        output_int32(int32(a_unwrap[1]))


def run() -> int32:
    f1()
    f2()
    f3()
    f4()
    f5()
    f6()
    f7()
    return 0
