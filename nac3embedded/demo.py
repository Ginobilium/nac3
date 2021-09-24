from language import *
from numpy import int32, int64


@syscall
def rtio_init():
    raise NotImplementedError("syscall not simulated")


@syscall
def rtio_get_counter() -> int64:
    raise NotImplementedError("syscall not simulated")


@syscall
def rtio_output(target: int32, data: int32):
    raise NotImplementedError("syscall not simulated")


@syscall
def rtio_input_timestamp(timeout_mu: int64, channel: int32) -> int64:
    raise NotImplementedError("syscall not simulated")


@syscall
def rtio_input_data(channel: int32) -> int32:
    raise NotImplementedError("syscall not simulated")


@kernel
class Demo:
    @kernel
    def run(self):
        rtio_init()


if __name__ == "__main__":
    Demo().run()
