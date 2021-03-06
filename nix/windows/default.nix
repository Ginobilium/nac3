{ pkgs }:
let
  msys2-env = pkgs.stdenvNoCC.mkDerivation rec {
    name = "msys2-env";
    srcs = import ./msys2_packages.nix { inherit pkgs; };
    buildInputs = [ pkgs.gnutar pkgs.zstd ];
    phases = [ "installPhase" ];
    installPhase = (pkgs.lib.strings.concatStringsSep "\n" (["mkdir $out"] ++ (map (p: "tar xvf ${p} -C $out") srcs)));
  };
  silenceFontconfig = # silence flood of "Fontconfig error: Cannot load default config file: No such file: (null)"
    ''
    export FONTCONFIG_PATH=$HOME/fonts
    mkdir $FONTCONFIG_PATH
    cat > $FONTCONFIG_PATH/fonts.conf << EOF
    <fontconfig>
    </fontconfig>
    EOF
    '';
  pyo3-mingw-config = pkgs.writeTextFile {
    name = "pyo3-mingw-config";
    text =
      ''
      implementation=CPython
      version=3.9
      shared=true
      abi3=false
      lib_name=python3.9
      lib_dir=${msys2-env}/mingw64/lib
      pointer_width=64
      build_flags=WITH_THREAD
      suppress_build_script_link_lines=false
      '';
  };
in rec {
  llvm-nac3 = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "llvm-nac3-msys2";
    version = "14.0.1";
    src-llvm = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/llvm-${version}.src.tar.xz";
      sha256 = "sha256-W4kBfewnKTEasUNALwPaHeptDHndXHAbyTnPizTwHsI=";
    };
    src-clang = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/clang-${version}.src.tar.xz";
      sha256 = "sha256-hE+O1cVEOPxTPQoW4KrPdfhLmKWaU9CEhT0tvsL9kqE=";
    };
    buildInputs = [ pkgs.wineWowPackages.stable ];
    phases = [ "unpackPhase" "patchPhase" "configurePhase" "buildPhase" "installPhase" ];
    unpackPhase =
      ''
      mkdir llvm
      tar xf ${src-llvm} -C llvm --strip-components=1
      mv llvm/Modules/* llvm/cmake/modules  # work around https://github.com/llvm/llvm-project/issues/53281
      mkdir clang
      tar xf ${src-clang} -C clang --strip-components=1
      cd llvm
      # build of llvm-lto fails and -DLLVM_BUILD_TOOLS=OFF does not disable it reliably because cmake
      rm -rf tools/lto
      '';
    patches = [ ../llvm/llvm-future-riscv-abi.diff ];
    configurePhase =
      ''
      export HOME=`mktemp -d`
      export WINEDEBUG=-all
      export WINEPATH=Z:${msys2-env}/mingw64/bin
      ${silenceFontconfig}
      mkdir build
      cd build
      wine64 cmake .. -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_UNWIND_TABLES=OFF -DLLVM_ENABLE_THREADS=OFF -DLLVM_TARGETS_TO_BUILD=X86\;ARM\;RISCV -DLLVM_LINK_LLVM_DYLIB=OFF -DLLVM_ENABLE_FFI=OFF -DFFI_INCLUDE_DIR=fck-cmake -DFFI_LIBRARY_DIR=fck-cmake -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_INSTALL_PREFIX=Z:$out
      '';
    buildPhase =
      ''
      wine64 ninja
      '';
    installPhase =
      ''
      wine64 ninja install
      '';
    dontFixup = true;
  };
  nac3artiq = pkgs.rustPlatform.buildRustPackage {
    name = "nac3artiq-msys2";
    src = ../../.;
    cargoLock = {
      lockFile = ../../Cargo.lock;
      outputHashes = {
        "inkwell-0.1.0" = "sha256-TPvmjByjSHs7U8W3oJU+y88IZcuc9WevymwNq6Ip1iA=";
      };
    };
    nativeBuildInputs = [ pkgs.wineWowPackages.stable ];
    buildPhase =
      ''
      export HOME=`mktemp -d`
      export WINEDEBUG=-all
      export WINEPATH=Z:${msys2-env}/mingw64/bin\;Z:${llvm-nac3}/bin
      ${silenceFontconfig}
      export PYO3_CONFIG_FILE=Z:${pyo3-mingw-config}
      wine64 cargo build --release -p nac3artiq
      '';
    installPhase =
      ''
      mkdir $out $out/nix-support
      cp target/release/nac3artiq.dll $out/nac3artiq.pyd
      echo file binary-dist $out/nac3artiq.pyd >> $out/nix-support/hydra-build-products
      '';
    checkPhase =
      ''
      wine64 cargo test --release
      '';
    dontFixup = true;
  };
  nac3artiq-pkg = pkgs.stdenvNoCC.mkDerivation {
    name = "nac3artiq-msys2-pkg";
    nativeBuildInputs = [ pkgs.pacman pkgs.fakeroot pkgs.libarchive pkgs.zstd ];
    src = nac3artiq;
    phases = [ "buildPhase" "installPhase" ];
    buildPhase =
      ''
      ln -s ${./PKGBUILD} PKGBUILD
      ln -s $src/nac3artiq.pyd nac3artiq.pyd
      makepkg --config ${./makepkg.conf} --nodeps
      '';
    installPhase =
      ''
      mkdir $out $out/nix-support
      cp *.pkg.tar.zst $out
      echo file msys2 $out/*.pkg.tar.zst >> $out/nix-support/hydra-build-products
      '';
  };
  lld = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "lld-msys2";
    version = "14.0.1";
    src = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/lld-${version}.src.tar.xz";
      sha256 = "sha256-MbrFSILSfJ4hfqRFA0BGrAUkLlyOSyxQ8/mAL8ijeBo=";
    };
    buildInputs = [ pkgs.wineWowPackages.stable ];
    phases = [ "unpackPhase" "patchPhase" "configurePhase" "buildPhase" "installPhase" ];
    patches = [ ./lld-disable-macho.diff ];
    setSourceRoot =  # work around https://github.com/llvm/llvm-project/issues/53281
      ''
      mv cmake/Modules/* lld-14.0.1.src/cmake/modules
      sourceRoot=lld-14.0.1.src
      '';
    configurePhase =
      ''
      export HOME=`mktemp -d`
      export WINEDEBUG=-all
      export WINEPATH=Z:${msys2-env}/mingw64/bin\;Z:${llvm-nac3}/bin
      ${silenceFontconfig}
      mkdir build
      cd build
      wine64 cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=Z:$out
      '';
    buildPhase =
      ''
      wine64 ninja
      '';
    installPhase =
      ''
      mkdir -p $out $out/nix-support
      cp bin/ld.lld.exe $out
      echo file binary-dist $out/ld.lld.exe >> $out/nix-support/hydra-build-products
      '';
    dontFixup = true;
  };
  wine-msys2 = pkgs.writeShellScriptBin "wine-msys2"
    ''
    export WINEDEBUG=-all
    export WINEPATH=Z:${msys2-env}/mingw64/bin\;Z:${llvm-nac3}/bin
    export PYO3_CONFIG_FILE=Z:${pyo3-mingw-config}
    exec ${pkgs.wineWowPackages.stable}/bin/wine64 cmd
    '';
  wine-msys2-build = pkgs.writeShellScriptBin "wine-msys2-build"
    ''
    export HOME=`mktemp -d`
    export WINEDEBUG=-all
    export WINEPATH=Z:${msys2-env}/mingw64/bin
    ${silenceFontconfig}
    exec ${pkgs.wineWowPackages.stable}/bin/wine64 $@
    '';
}
