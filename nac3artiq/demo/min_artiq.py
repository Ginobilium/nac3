from inspect import getfullargspec
from functools import wraps
from types import SimpleNamespace
from numpy import int32, int64
from typing import Generic, TypeVar
from math import floor, ceil

import nac3artiq
from embedding_map import EmbeddingMap


__all__ = [
    "Kernel", "KernelInvariant", "virtual",
    "Option", "Some", "none", "UnwrapNoneError",
    "round64", "floor64", "ceil64",
    "extern", "kernel", "portable", "nac3",
    "rpc", "ms", "us", "ns",
    "print_int32", "print_int64",
    "Core", "TTLOut",
    "parallel", "sequential"
]


T = TypeVar('T')

class Kernel(Generic[T]):
    pass

class KernelInvariant(Generic[T]):
    pass

# The virtual class must exist before nac3artiq.NAC3 is created.
class virtual(Generic[T]):
    pass

class Option(Generic[T]):
    _nac3_option: T

    def __init__(self, v: T):
        self._nac3_option = v

    def is_none(self):
        return self._nac3_option is None
    
    def is_some(self):
        return not self.is_none()
    
    def unwrap(self):
        if self.is_none():
            raise UnwrapNoneError()
        return self._nac3_option

    def __repr__(self) -> str:
        if self.is_none():
            return "none"
        else:
            return "Some({})".format(repr(self._nac3_option))
    
    def __str__(self) -> str:
        if self.is_none():
            return "none"
        else:
            return "Some({})".format(str(self._nac3_option))

def Some(v: T) -> Option[T]:
    return Option(v)

none = Option(None)

def round64(x):
    return round(x)

def floor64(x):
    return floor(x)

def ceil64(x):
    return ceil(x)


import device_db
core_arguments = device_db.device_db["core"]["arguments"]

compiler = nac3artiq.NAC3(core_arguments["target"])
allow_registration = True
# Delay NAC3 analysis until all referenced variables are supposed to exist on the CPython side.
registered_functions = set()
registered_classes = set()

def register_function(fun):
    assert allow_registration
    registered_functions.add(fun)

def register_class(cls):
    assert allow_registration
    registered_classes.add(cls)


def extern(function):
    """Decorates a function declaration defined by the core device runtime."""
    register_function(function)
    return function

def rpc(function):
    """Decorates a function declaration defined by the core device runtime."""
    register_function(function)
    return function

def kernel(function_or_method):
    """Decorates a function or method to be executed on the core device."""
    register_function(function_or_method)
    argspec = getfullargspec(function_or_method)
    if argspec.args and argspec.args[0] == "self":
        @wraps(function_or_method)
        def run_on_core(self, *args, **kwargs):
            fake_method = SimpleNamespace(__self__=self, __name__=function_or_method.__name__)
            self.core.run(fake_method, *args, **kwargs)
    else:
        @wraps(function_or_method)
        def run_on_core(*args, **kwargs):
            raise RuntimeError("Kernel functions need explicit core.run()")
    return run_on_core


def portable(function):
    """Decorates a function or method to be executed on the same device (host/core device) as the caller."""
    register_function(function)
    return function


def nac3(cls):
    """
    Decorates a class to be analyzed by NAC3.
    All classes containing kernels or portable methods must use this decorator.
    """
    register_class(cls)
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


# These is not part of ARTIQ and only available in runkernel. Defined here for convenience.
@extern
def print_int32(x: int32):
    raise NotImplementedError("syscall not simulated")


@extern
def print_int64(x: int64):
    raise NotImplementedError("syscall not simulated")


@nac3
class Core:
    ref_period: KernelInvariant[float]

    def __init__(self):
        self.ref_period = core_arguments["ref_period"]

    def run(self, method, *args, **kwargs):
        global allow_registration

        embedding = EmbeddingMap()

        if allow_registration:
            compiler.analyze(registered_functions, registered_classes)
            allow_registration = False

        if hasattr(method, "__self__"):
            obj = method.__self__
            name = method.__name__
        else:
            obj = method
            name = ""

        compiler.compile_method_to_file(obj, name, args, "module.elf", embedding)

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
    core: KernelInvariant[Core]
    channel: KernelInvariant[int32]
    target_o: KernelInvariant[int32]

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

@nac3
class UnwrapNoneError(Exception):
    """raised when unwrapping a none value"""
    artiq_builtin = True

parallel = KernelContextManager()
sequential = KernelContextManager()
