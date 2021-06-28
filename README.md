# nac3 compiler

This repository contains:
- nac3core: Core compiler library, containing type-checking, static analysis (in
    the future) and code generation.
- nac3embedded: Integration with CPython runtime.
- nac3standalone: Standalone compiler tool.

The core compiler would know nothing about symbol resolution, host variables
etc. The nac3embedded/nac3standalone library would provide (implement) the
symbol resolver to the core compiler for resolving the type and value for
unknown symbols. The core compiler would only type check classes and functions
requested by the nac3embedded/nac3standalone lib (the API should allow the
caller to specify which methods should be compiled). After type checking, the
compiler would analyse the set of functions/classes that are used and perform
code generation.

value could be integer values, boolean values, bytes (for memcpy), function ID
(full name + concrete type)

## Current Plan

Type checking:

- [x] Basic interface for symbol resolver.
- [x] Track location information in context object (for diagnostics).
- [ ] Refactor old expression and statement type inference code. (anto)
- [ ] Error diagnostics utilities. (pca)
- [ ] Move tests to external files, write scripts for testing. (pca)
- [ ] Implement function type checking (instantiate bounded type parameters),
    loop unrolling, type inference for lists with virtual objects. (pca)



