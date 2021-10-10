from min_artiq import *
from numpy import int32, int64


@kernel
class Demo:
    core: Core
    led: TTLOut

    @portable
    def __init__(self):
        self.core = Core()
        self.led = TTLOut(0)

    @kernel
    def run(self):
        self.core.reset()
        while True:
            self.led.pulse_mu(int64(100000000))
            delay_mu(int64(True))


@kernel
def testing(a: int32) -> int32:
    return a + 1

if __name__ == "__main__":
    core = Core()
    # core.run(testing, 1)
    core.run(Demo().run)
