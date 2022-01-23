@extern
def output_int32(x: int32):
    ...

@extern
def output_int64(x: int64):
    ...


class B:
    b: int32
    def __init__(self, a: int32):
        self.b = a


class A:
    a: int32
    b: B
    def __init__(self, a: int32):
        self.a = a
        self.b = B(a + 1)

    def get_a(self) -> int32:
        return self.a
    
    def get_b(self) -> B:
        return self.b


def run() -> int32:
    a = A(10)
    output_int32(a.a)

    a = A(20)
    output_int32(a.a)
    output_int32(a.get_a())
    # output_int32(a.get_b().b) FIXME: NAC3 prints garbage
    return 0
