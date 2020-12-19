from functools import wraps

import nac3embedded


__all__ = ["kernel", "portable"]


def kernel(function):
    @wraps(function)
    def run_on_core(self, *args, **kwargs):
        nac3 = nac3embedded.NAC3()
        nac3.register_host_object(self)
        nac3.compile_method(self, function.__name__)
    return run_on_core


def portable(function):
    return fn
