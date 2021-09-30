from language import *
from numpy import int32, int64

@extern
def now_mu() -> int64:
    raise NotImplementedError("syscall not simulated")


@extern
def at_mu(t: int64):
    raise NotImplementedError("syscall not simulated")


@extern
def delay_mu(dt: int64):
    raise NotImplementedError("syscall not simulated")


@extern
def rtio_init():
    raise NotImplementedError("syscall not simulated")


@extern
def rtio_get_counter() -> int64:
    raise NotImplementedError("syscall not simulated")


@extern
def rtio_output(target: int32, data: int32):
    raise NotImplementedError("syscall not simulated")


@extern
def rtio_input_timestamp(timeout_mu: int64, channel: int32) -> int64:
    raise NotImplementedError("syscall not simulated")


@extern
def rtio_input_data(channel: int32) -> int32:
    raise NotImplementedError("syscall not simulated")
