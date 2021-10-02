from numpy import int32, int64
from language import *


__all__ = ["now_mu", "at_mu", "delay_mu"]


@extern
def now_mu() -> int64:
    raise NotImplementedError("syscall not simulated")


@extern
def at_mu(t: int64):
    raise NotImplementedError("syscall not simulated")


@extern
def delay_mu(dt: int64):
    raise NotImplementedError("syscall not simulated")
