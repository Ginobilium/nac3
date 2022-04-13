@extern
def output_int32(x: int32):
    ...

class A:
    def foo(self):
        output_int32(1)

def run() -> int32:
    inst = A()
    inst.foo()
    output_int32(1)
    return 0
