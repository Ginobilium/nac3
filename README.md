# NAC3 compiler

This repository contains:
- nac3ast: Python abstract syntax tree definition (based on RustPython).
- nac3parser: Python parser (based on RustPython).
- nac3core: Core compiler library, containing type-checking and code
  generation.
- nac3standalone: Standalone compiler tool (core language only).
- nac3artiq: Integration with ARTIQ and implementation of ARTIQ-specific
  extensions to the core language.
- runkernel: Simple program that runs compiled ARTIQ kernels on the host
  and displays RTIO operations. Useful for testing without hardware.


The core compiler knows nothing about symbol resolution, host variables
etc. nac3artiq and nac3standalone provide (implement) the
symbol resolver to the core compiler for resolving the type and value for
unknown symbols. The core compiler only type checks classes and functions
requested by nac3artiq/nac3standalone (the API should allow the
caller to specify which methods should be compiled). After type checking, the
compiler analyses the set of functions/classes that are used and performs
code generation.

value could be integer values, boolean values, bytes (for memcpy), function ID
(full name + concrete type)
