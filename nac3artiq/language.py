from inspect import isclass, getmodule
from functools import wraps

import nac3artiq

import device_db


__all__ = ["extern", "kernel"]


nac3 = nac3artiq.NAC3(device_db.device_db["core"]["arguments"]["target"])
allow_module_registration = True


def extern(function):
    assert allow_module_registration
    nac3.register_module(getmodule(function))
    return function


def kernel(class_or_function):
    global allow_module_registration

    assert allow_module_registration
    nac3.register_module(getmodule(class_or_function))
    if isclass(class_or_function):
        return class_or_function
    else:
        @wraps(class_or_function)
        def run_on_core(self, *args, **kwargs):
            global allow_module_registration
            if allow_module_registration:
                nac3.analyze()
                allow_module_registration = False
            nac3.compile_method(self.__class__.__name__, class_or_function.__name__)
        return run_on_core
