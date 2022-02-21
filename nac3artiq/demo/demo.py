from min_artiq import *
from numpy import int32, int64

@extern
def output_int(x: int32):
    ...


class InexistingException(Exception):
    pass

@nac3
class Demo:
    core: KernelInvariant[Core]
    led0: KernelInvariant[TTLOut]
    led1: KernelInvariant[TTLOut]

    def __init__(self):
        self.core = Core()
        self.led0 = TTLOut(self.core, 18)
        self.led1 = TTLOut(self.core, 19)

    @kernel
    def test(self):
        a = (1, True)
        a[0]()

    @kernel
    def test2(self):
        a = (1, True)
        output_int(int32(a))

    @kernel
    def run(self):
        self.core.reset()
        while True:
            with parallel:
                self.led0.pulse(100.*ms)
                self.led1.pulse(100.*ms)
            self.core.delay(100.*ms)


if __name__ == "__main__":
    Demo().run()
