#!/usr/bin/env bash

set -e

cd $(dirname $0)
MSYS2DIR=`pwd`/msys2
mkdir -p $MSYS2DIR/var/lib/pacman $MSYS2DIR/msys/etc

curl -L https://mirror.msys2.org/msys/x86_64/pacman-mirrors-20220205-1-any.pkg.tar.zst | tar xvf - -C $MSYS2DIR --zstd
curl -L https://raw.githubusercontent.com/msys2/MSYS2-packages/master/pacman/pacman.conf | grep -v SigLevel | sed s\|/etc/pacman.d\|$MSYS2DIR/etc/pacman.d\|g > $MSYS2DIR/etc/pacman.conf

fakeroot pacman --root $MSYS2DIR --config $MSYS2DIR/etc/pacman.conf -Syy
pacman --root $MSYS2DIR --config $MSYS2DIR/etc/pacman.conf --cachedir $MSYS2DIR/msys/cache -Sp mingw-w64-x86_64-rust mingw-w64-x86_64-cmake mingw-w64-x86_64-ninja mingw-w64-x86_64-python3.9 mingw-w64-x86_64-python-numpy > $MSYS2DIR/packages.txt

echo "{ pkgs } : [" > msys2_packages.nix
while read package; do
	hash=$(nix-prefetch-url $package)
	echo "
(pkgs.fetchurl {
  url = \"$package\";
  sha256 = \"$hash\";
})" >> msys2_packages.nix
done < $MSYS2DIR/packages.txt
echo "]" >> msys2_packages.nix
