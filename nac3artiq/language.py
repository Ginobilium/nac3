from inspect import isclass
from functools import wraps

import nac3artiq


__all__ = ["extern", "kernel"]


nac3 = nac3artiq.NAC3()
allow_object_registration = True


def extern(function):
    assert allow_object_registration
    nac3.register_object(function)
    return function


def kernel(function_or_class):
    global allow_object_registration

    if isclass(function_or_class):
        assert allow_object_registration
        nac3.register_object(function_or_class)
        return function_or_class
    else:
        @wraps(function_or_class)
        def run_on_core(self, *args, **kwargs):
            global allow_object_registration
            if allow_object_registration:
                nac3.analyze()
                allow_object_registration = False
            nac3.compile_method(self.__class__.__name__, function_or_class.__name__)
        return run_on_core
