from language import *
from artiq_builtins import *
from numpy import int32, int64


@kernel
class Core:
    @kernel
    def reset(self):
        rtio_init()
        at_mu(rtio_get_counter() + int64(125000))

    @kernel
    def break_realtime(self):
        min_now = rtio_get_counter() + int64(125000)
        if now_mu() < min_now:
            at_mu(min_now)

@kernel
class TTLOut:
    channel: int32
    target_o: int32

    @kernel
    def __init__(self, channel: int32):
        self.channel = channel
        self.target_o = channel << 8

    @kernel
    def output(self):
        pass

    @kernel
    def set_o(self, o: bool):
        rtio_output(self.target_o, 1 if o else 0)

    @kernel
    def on(self):
        self.set_o(True)

    @kernel
    def off(self):
        self.set_o(False)

    @kernel
    def pulse_mu(self, duration: int64):
        self.on()
        delay_mu(duration)
        self.off()


@kernel
class Demo:
    @kernel
    def run(self):
        core = Core()
        led = TTLOut(0)

        core.reset()
        while True:
            led.pulse_mu(int64(100000000))
            delay_mu(int64(100000000))


if __name__ == "__main__":
    Demo().run()
