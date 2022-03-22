{ pkgs }:
let
  msys2-env = pkgs.stdenvNoCC.mkDerivation rec {
    name = "msys2-env";
    srcs = import ./msys2_packages.nix { inherit pkgs; };
    buildInputs = [ pkgs.gnutar pkgs.zstd ];
    phases = [ "installPhase" ];
    installPhase = (pkgs.lib.strings.concatStringsSep "\n" (["mkdir $out"] ++ (map (p: "tar xvf ${p} -C $out") srcs)));
  };
in
{
  llvm-nac3 = pkgs.stdenvNoCC.mkDerivation rec {
    name = "llvm-nac3-msys2";
    src-llvm = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.1/llvm-13.0.1.src.tar.xz";
      sha256 = "sha256-7GuA2Cw4SsrS3BkpA6bPLNuv+4ibhL+5janXHmMPyDQ=";
    };
    src-clang = pkgs.fetchurl {
      url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.1/clang-13.0.1.src.tar.xz";
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
      export WINEPATH=${msys2-env}/mingw64/bin

      # silence flood of "Fontconfig error: Cannot load default config file: No such file: (null)"
      export FONTCONFIG_PATH=$HOME/fonts
      mkdir $FONTCONFIG_PATH
      cat > $FONTCONFIG_PATH/fonts.conf << EOF
      <fontconfig>
      </fontconfig>
      EOF

      mkdir build
      cd build
      wine64 cmake .. -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_UNWIND_TABLES=OFF -DLLVM_ENABLE_THREADS=OFF -DLLVM_TARGETS_TO_BUILD=X86\;ARM\;RISCV -DLLVM_LINK_LLVM_DYLIB=OFF -DLLVM_ENABLE_FFI=OFF -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_INSTALL_PREFIX=$out
      '';
    buildPhase =
      ''
      wine64 ninja
      '';
    installPhase =
      ''
      wine64 ninja install
      '';
  };
}
