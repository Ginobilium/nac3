from typing import TypeVar, Generic

@extern
def output_int32(x: int32):
    ...

class A:
    def __init__(self):
        pass

    def foo(self):
        output_int32(1)

class B:
    def __init__(self):
        pass

    def foo(self):
        output_int32(2)


T = TypeVar("T", A, B)


class C(Generic[T]):
    x: T

    def __init__(self, x: T):
        self.x = x

    def foo(self):
        self.x.foo()


def run() -> int32:
    insta = A()
    inst = C(insta)
    inst.foo()
    return 0

