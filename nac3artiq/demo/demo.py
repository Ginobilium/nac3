from min_artiq import *

@nac3
class Demo:
    core: Core
    led0: TTLOut
    led1: TTLOut

    def __init__(self):
        self.core = Core()
        self.led0 = TTLOut(self.core, 18)
        self.led1 = TTLOut(self.core, 19)

    @kernel
    def run_k(self):
        self.core.reset()
        while True:
            with parallel:
                self.led0.pulse(100.*ms)
                self.led1.pulse(100.*ms)
            self.core.delay(100.*ms)

    def run(self):
        self.core.run(self.run_k)


if __name__ == "__main__":
    Demo().run()