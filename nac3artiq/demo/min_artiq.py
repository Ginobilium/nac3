from inspect import isclass, getmodule
from functools import wraps
import sys
from numpy import int32, int64

import nac3artiq

__all__ = ["KernelInvariant", "extern", "kernel", "portable", "nac3",
           "ms", "us", "ns",
           "Core", "TTLOut", "parallel", "sequential"]


import device_db
core_arguments = device_db.device_db["core"]["arguments"]

compiler = nac3artiq.NAC3(core_arguments["target"])
allow_module_registration = True
registered_ids = set()

def KernelInvariant(t):
    return t


def register_module_of(obj):
    global registered_ids
    assert allow_module_registration
    module = getmodule(obj)
    module_id = id(module)
    if module_id not in registered_ids:
        compiler.register_module(module)
        registered_ids.add(module_id)


def extern(function):
    """Decorates a function declaration defined by the core device runtime."""
    register_module_of(function)
    return function


def kernel(class_or_function):
    """Decorates a function or method to be executed on the core device."""
    register_module_of(class_or_function)
    @wraps(class_or_function)
    def device_only(*args, **kwargs):
        raise RuntimeError("Kernels must not be called directly, use core.run(kernel_function) instead")
    return device_only


def portable(function):
    """Decorates a function or method to be executed on the same device (host/core device) as the caller."""
    register_module_of(function)
    return function


def nac3(cls):
    """
    Decorates a class to be analyzed by NAC3.
    All classes containing kernels or portable methods must use this decorator.
    """
    register_module_of(cls)
    return cls


ms = 1e-3
us = 1e-6
ns = 1e-9

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


@nac3
class Core:
    ref_period: float

    def __init__(self):
        self.ref_period = core_arguments["ref_period"]

    def run(self, method, *args, **kwargs):
        global allow_module_registration
        if allow_module_registration:
            compiler.analyze()
            allow_module_registration = False

        if hasattr(method, "__self__"):
            obj = method.__self__
            name = method.__name__
        else:
            obj = method
            name = ""

        compiler.compile_method_to_file(obj, name, args, "module.elf")

    @kernel
    def reset(self):
        rtio_init()
        at_mu(rtio_get_counter() + int64(125000))

    @kernel
    def break_realtime(self):
        min_now = rtio_get_counter() + int64(125000)
        if now_mu() < min_now:
            at_mu(min_now)

    @portable
    def seconds_to_mu(self, seconds: float) -> int64:
        return int64(round(seconds/self.ref_period))

    @portable
    def mu_to_seconds(self, mu: int64) -> float:
        return float(mu)*self.ref_period

    @kernel
    def delay(self, dt: float):
        delay_mu(self.seconds_to_mu(dt))


@nac3
class TTLOut:
    core: Core
    channel: int32
    target_o: int32

    @portable
    def __init__(self, core: Core, channel: int32):
        self.core = core
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
    def pulse(self, duration: float):
        self.on()
        self.core.delay(duration)
        self.off()

@nac3
class KernelContextManager:
    @kernel
    def __enter__(self):
        pass

    @kernel
    def __exit__(self):
        pass

parallel = KernelContextManager()
sequential = KernelContextManager()
