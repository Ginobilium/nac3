from functools import wraps

import nac3embedded


__all__ = ["kernel", "portable"]


def kernel(function):
    @wraps(function)
    def run_on_core(self, *args, **kwargs):
        nac3embedded.add_host_object(self)
    return run_on_core


def portable(function):
    return fn
