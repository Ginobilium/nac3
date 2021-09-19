class A:
    a: int32
    def __init__(self, a: int32):
        self.a = a

    def get_a(self) -> int32:
        return self.a

    def get_self(self) -> A:
        return self

def run() -> int32:
    a = A(10)
    output(a.a)

    a = A(20)
    output(a.a)
    output(a.get_a())
    return 0

