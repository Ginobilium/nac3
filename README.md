![icon](https://git.m-labs.hk/M-Labs/nac3/raw/branch/master/nac3.svg)

# NAC3

NAC3 is a major, backward-incompatible rewrite of the compiler for the [ARTIQ](https://m-labs.hk/artiq) physics experiment control and data acquisition system. It features greatly improved compilation speeds, a much better type system, and more predictable and transparent operation.

NAC3 has a modular design and its applicability reaches beyond ARTIQ. The ``nac3core`` module does not contain anything specific to ARTIQ, and can be used in any project that requires compiling Python to machine code.

**WARNING: NAC3 is currently experimental software and several important features are not implemented yet.**

## Packaging

NAC3 is packaged using the [Nix](https://nixos.org) Flakes system. Install Nix 2.4+ and enable flakes by adding ``experimental-features = nix-command flakes`` to ``nix.conf`` (e.g. ``~/.config/nix/nix.conf``).

## Try NAC3

### Linux

After setting up Nix as above, use ``nix shell git+https://github.com/m-labs/artiq.git?ref=nac3`` to get a shell with the NAC3 version of ARTIQ. See the ``examples`` directory in ARTIQ (``nac3`` Git branch) for some samples of NAC3 kernel code.

### Windows

Install [MSYS2](https://www.msys2.org/), and open "MSYS2 MinGW x64". Edit ``/etc/pacman.conf`` to add:
```
[artiq]
SigLevel = Optional TrustAll
Server = https://lab.m-labs.hk/msys2
```

Then run the following commands:
```
pacman -Syu
pacman -S mingw-w64-x86_64-artiq
```

Install ``lld-msys2`` manually:
```
wget https://nixbld.m-labs.hk/build/115527/download/1/ld.lld.exe
mv ld.lld.exe C:/msys64/mingw64/bin
```

Note: This build of NAC3 cannot be used with Anaconda Python nor the python.org binaries for Windows. Those Python versions are compiled with Visual Studio (MSVC) and their ABI is incompatible with the GNU ABI used in this build. We have no plans to support Visual Studio nor the MSVC ABI. If you need a MSVC build, please install the requisite bloated spyware from Microsoft and compile NAC3 yourself.

## For developers

This repository contains:
- ``nac3ast``: Python abstract syntax tree definition (based on RustPython).
- ``nac3parser``: Python parser (based on RustPython).
- ``nac3core``: Core compiler library, containing type-checking and code generation.
- ``nac3standalone``: Standalone compiler tool (core language only).
- ``nac3artiq``: Integration with ARTIQ and implementation of ARTIQ-specific extensions to the core language.
- ``runkernel``: Simple program that runs compiled ARTIQ kernels on the host and displays RTIO operations. Useful for testing without hardware.

Use ``nix develop`` in this repository to enter a development shell.
If you are using a different shell than bash you can use e.g. ``nix develop --command fish``.

Build NAC3 with ``cargo build --release``. See the demonstrations in ``nac3artiq`` and ``nac3standalone``.
