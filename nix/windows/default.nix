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
    version = "13.0.1";
    src-llvm = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/llvm-${version}.src.tar.xz";
      sha256 = "sha256-7GuA2Cw4SsrS3BkpA6bPLNuv+4ibhL+5janXHmMPyDQ=";
    };
    src-clang = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/clang-${version}.src.tar.xz";
      sha256 = "sha256-eHqeLZn1yHIKoXc+S+AJRhzTDTvUD90kWR5HNGfJF8k=";
    };
    buildInputs = [ pkgs.wineWowPackages.stable ];
    phases = [ "unpackPhase" "patchPhase" "configurePhase" "buildPhase" "installPhase" ];
    unpackPhase =
      ''
      mkdir llvm
      tar xf ${src-llvm} -C llvm --strip-components=1
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
      wine64 cmake .. -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_UNWIND_TABLES=OFF -DLLVM_ENABLE_THREADS=OFF -DLLVM_TARGETS_TO_BUILD=X86\;ARM\;RISCV -DLLVM_LINK_LLVM_DYLIB=OFF -DLLVM_ENABLE_FFI=OFF -DFFI_INCLUDE_DIR=fck-cmake -DFFI_LIBRARY_DIR=fck-cmake -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_INSTALL_PREFIX=Z:$out
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
    cargoLock = { lockFile = ../../Cargo.lock; };
    nativeBuildInputs = [ pkgs.wineWowPackages.stable pkgs.zip ];
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
      mkdir -p $out $out/nix-support
      ln -s target/release/nac3artiq.dll nac3artiq.pyd
      zip $out/nac3artiq.zip nac3artiq.pyd
      echo file binary-dist $out/nac3artiq.zip >> $out/nix-support/hydra-build-products
      '';
    checkPhase =
      ''
      wine64 cargo test --release
      '';
    dontFixup = true;
  };
  lld = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "lld-msys2";
    version = "13.0.1";
    src = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/lld-${version}.src.tar.xz";
      sha256 = "sha256-Zmr3Rei/e2gFM7TRi3ox3HyrV1sebk0mGSK7r9lkTPs=";
    };
    buildInputs = [ pkgs.wineWowPackages.stable ];
    phases = [ "unpackPhase" "patchPhase" "configurePhase" "buildPhase" "installPhase" ];
    patches = [ ./lld-disable-macho.diff ];
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
      wine64 ninja install
      mkdir $out/nix-support
      echo file binary-dist $out/bin/lld.exe >> $out/nix-support/hydra-build-products
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
}
