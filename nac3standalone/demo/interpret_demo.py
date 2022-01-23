#!/usr/bin/env python3

import sys
import importlib.util
import importlib.machinery
import pathlib

from numpy import int32, int64


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
        elif name in {"output_int32", "output_int64", "output_int32_list"}:
            return print
        else:
            raise NotImplementedError

    module.int32 = int32
    module.int64 = int64
    module.extern = extern


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
