from min_artiq import *

@kernel
class Demo:
    core: Core
    led: TTLOut

    def __init__(self):
        self.core = Core()
        self.led = TTLOut(self.core, 19)

    @kernel
    def run_k(self):
        self.core.reset()
        while True:
            self.led.pulse(100.*ms)
            self.core.delay(100.*ms)

    def run(self):
        self.core.run(self.run_k)


if __name__ == "__main__":
    Demo().run()
