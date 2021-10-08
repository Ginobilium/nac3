from min_artiq import *
from numpy import int32, int64


@kernel
class Demo:
    core: Core
    led: TTLOut

    def __init__(self):
        self.core = Core()
        self.led = TTLOut(0)

    @kernel
    def main_kernel(self):
        self.core.reset()
        while True:
            self.led.pulse_mu(int64(100000000))
            delay_mu(int64(100000000))

    def run(self):
        self.core.run(self.main_kernel)


if __name__ == "__main__":
    Demo().run()
