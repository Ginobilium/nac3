# NAC3

NAC3 is a major, backward-incompatible rewrite of the compiler for the [ARTIQ](https://m-labs.hk/artiq) physics experiment control and data acquisition system. It features greatly improved compilation speeds, a much better type system, and more predictable and transparent operation.

NAC3 has a modular design and its applicability reaches beyond ARTIQ. The ``nac3core`` module does not contain anything specific to ARTIQ, and can be used in any project that requires compiling Python to machine code.

**WARNING: NAC3 is currently experimental software and several important features are not implemented yet.**

## Packaging

NAC3 is packaged using the [Nix](https://nixos.org) Flakes system. Install Nix 2.4+ and enable flakes by adding ``experimental-features = nix-command flakes`` to ``nix.conf`` (e.g. ``~/.config/nix/nix.conf``).

## Try NAC3

### Linux

After setting up Nix as above, use ``nix shell git+https://github.com/m-labs/artiq.git?ref=nac3`` to get a shell with the NAC3 version of ARTIQ. See the ``examples`` directory in ARTIQ (``nac3`` Git branch) for some samples of NAC3 kernel code.

### Windows (work in progress)

NAC3 ARTIQ packaging for MSYS2/Windows is not yet complete so installation involves many manual steps. It is also less tested and you may encounter problems.

Install [MSYS2](https://www.msys2.org/) and run the following commands:
```
pacman -S mingw-w64-x86_64-python-h5py mingw-w64-x86_64-python-pyqt5 mingw-w64-x86_64-python-scipy mingw-w64-x86_64-python-prettytable mingw-w64-x86_64-python-pygit2
pacman -S mingw-w64-x86_64-python-pip
pip install qasync
pip install pyqtgraph
pacman -S patch git
git clone https://github.com/m-labs/sipyco
cd sipyco
git show 20c946aad78872fe60b78d9b57a624d69f3eea47 | patch -p1 -R
python setup.py install
cd ..
git clone -b nac3 https://github.com/m-labs/artiq
cd artiq
python setup.py install
```

Locate a recent build of ``nac3artiq-mingw`` from [Hydra](https://nixbld.m-labs.hk) and download ``nac3artiq.zip``. Then extract the contents in the appropriate location:
```
pacman -S unzip
wget https://nixbld.m-labs.hk/build/97899/download/1/nac3artiq.zip  # edit the build number
unzip nac3artiq.zip -d C:/msys64/mingw64/lib/python3.9/site-packages
```

Install additional NAC3 dependencies:
```
pacman -S mingw-w64-x86_64-lld
wget https://nixbld.m-labs.hk/build/97899/download/1/mcfgthread-12.dll  # edit the build number
cp mcfgthread-12.dll C:/msys64/mingw64/bin
```

And you should be good to go.

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
