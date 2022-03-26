#!/usr/bin/env python3

import sys
import importlib.util
import importlib.machinery
import pathlib

from numpy import int32, int64, uint32, uint64
from typing import TypeVar, Generic

T = TypeVar('T')
class Option(Generic[T]):
    _nac3_option: T

    def __init__(self, v: T):
        self._nac3_option = v

    def is_none(self):
        return self._nac3_option is None
    
    def is_some(self):
        return not self.is_none()
    
    def unwrap(self):
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

def patch(module):
    def output_asciiart(x):
        if x < 0:
            sys.stdout.write("\n")
        else:
            sys.stdout.write(" .,-:;i+hHM$*#@  "[x])

    def extern(fun):
        name = fun.__name__
        if name == "output_asciiart":
            return output_asciiart
        elif name in {
            "output_int32",
            "output_int64",
            "output_int32_list",
            "output_uint32",
            "output_uint64",
            "output_float64"
        }:
            return print
        else:
            raise NotImplementedError

    module.int32 = int32
    module.int64 = int64
    module.uint32 = uint32
    module.uint64 = uint64
    module.TypeVar = TypeVar
    module.Generic = Generic
    module.extern = extern
    module.Option = Option
    module.Some = Some
    module.none = none


def file_import(filename, prefix="file_import_"):
    filename = pathlib.Path(filename)
    modname = prefix + filename.stem

    path = str(filename.resolve().parent)
    sys.path.insert(0, path)

    try:
        spec = importlib.util.spec_from_loader(
            modname,
            importlib.machinery.SourceFileLoader(modname, str(filename)),
        )
        module = importlib.util.module_from_spec(spec)
        patch(module)
        spec.loader.exec_module(module)
    finally:
        sys.path.remove(path)

    return module


def main():
    demo = file_import(sys.argv[1])
    demo.run()


if __name__ == "__main__":
    main()
