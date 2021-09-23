from inspect import isclass
from functools import wraps

import nac3embedded


__all__ = ["kernel"]


nac3 = nac3embedded.NAC3()
allow_class_registration = True


def kernel(function_or_class):
    global allow_class_registration

    if isclass(function_or_class):
        assert allow_class_registration
        nac3.register_class(function_or_class)
        return function_or_class
    else:
        @wraps(function_or_class)
        def run_on_core(self, *args, **kwargs):
            global allow_class_registration
            if allow_class_registration:
                nac3.analyze()
                allow_class_registration = False
            nac3.compile_method(self.__class__.__name__, function_or_class.__name__)
        return run_on_core
