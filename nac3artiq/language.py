from inspect import isclass, getmodule
from functools import wraps
import sys

import nac3artiq

import device_db


__all__ = ["extern", "kernel", "run_on_core"]


nac3 = nac3artiq.NAC3(device_db.device_db["core"]["arguments"]["target"])
allow_module_registration = True
registered_ids = set()

def extern(function):
    global registered_ids
    assert allow_module_registration
    module = getmodule(function)
    module_id = id(module)
    if module_id not in registered_ids:
        nac3.register_module(module)
        registered_ids.add(module_id)
    return function


def kernel(class_or_function):
    global allow_module_registration
    global registered_ids

    assert allow_module_registration
    module = getmodule(class_or_function)
    module_id = id(module)
    if module_id not in registered_ids:
        nac3.register_module(module)
        registered_ids.add(module_id)

    return class_or_function

def get_defined_class(method):
    return vars(sys.modules[method.__module__])[method.__qualname__.split('.')[0]]

def run_on_core(method, *args, **kwargs):
    global allow_module_registration
    if allow_module_registration:
        nac3.analyze()
        allow_module_registration = False
    nac3.compile_method(id(get_defined_class(method)), method.__name__)

