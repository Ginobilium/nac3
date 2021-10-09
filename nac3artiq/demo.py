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
            delay_mu(int64(100000000))


@kernel
class Workaround56:
    @kernel
    def run(self):
        demo = Demo()
        demo.run()

    def run_host(self):
        core = Core()
        core.run(self.run) # works because run() never uses its self argument

if __name__ == "__main__":
    Workaround56().run_host()
