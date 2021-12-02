{
  description = "The third-generation ARTIQ compiler";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/0f4b4b85d959200f52c16bbb74036994e7db5f74;

  outputs = { self, nixpkgs }:
    let
      # We can't use overlays because llvm dependencies are handled internally in llvmPackages_xx
      pkgs-orig = import nixpkgs { system = "x86_64-linux"; };
      nixpkgs-patched = pkgs-orig.applyPatches {
        name = "nixpkgs";
        src = nixpkgs;
        patches = [ ./llvm-future-riscv-abi.diff ./llvm-restrict-targets.diff ./llvm-mingw-crosscompile.diff ./llvm-unbreak-static-cross.diff ];
      };
      pkgs = import nixpkgs-patched { system = "x86_64-linux"; };
      pkgs-mingw = import nixpkgs-patched { system = "x86_64-linux"; crossSystem = { config = "x86_64-w64-mingw32"; libc = "msvcrt"; }; };
      cargoSha256 = "sha256-otKLhr58HYMjVXAof6AdObNpggPnvK6qOl7I+4LWIP8=";
      msys2-python-tar = pkgs.fetchurl {
        url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-python-3.9.7-4-any.pkg.tar.zst";
        sha256 = "0iwlgbk4b457yn9djwqswid55xhyyi35qymz1lfh42xwdpxdm47c";
      };
      msys2-python = pkgs.runCommand "msys2-python" { buildInputs = [ pkgs.gnutar pkgs.zstd ]; }
        ''
        mkdir $out
        tar xvf ${msys2-python-tar} -C $out
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
          lib_dir=${msys2-python}/mingw64/lib
          pointer_width=64
          build_flags=WITH_THREAD
          suppress_build_script_link_lines=false
          '';
      };
    in rec {
      inherit nixpkgs-patched;

      packages.x86_64-linux = {
        nac3artiq = pkgs.python3Packages.toPythonModule (
          pkgs.rustPlatform.buildRustPackage {
            name = "nac3artiq";
            src = self;
            inherit cargoSha256;
            nativeBuildInputs = [ pkgs.python3 pkgs.llvm_12 ];
            buildInputs = [ pkgs.python3 pkgs.libffi pkgs.libxml2 pkgs.llvm_12 ];
            cargoBuildFlags = [ "--package" "nac3artiq" ];
            cargoTestFlags = [ "--package" "nac3ast" "--package" "nac3parser" "--package" "nac3core" "--package" "nac3artiq" ];
            installPhase =
              ''
              TARGET_DIR=$out/${pkgs.python3Packages.python.sitePackages}
              mkdir -p $TARGET_DIR
              cp target/x86_64-unknown-linux-gnu/release/libnac3artiq.so $TARGET_DIR/nac3artiq.so
              '';
          }
        );
      };

      packages.x86_64-w64-mingw32 = {
        nac3artiq = pkgs-mingw.python3Packages.toPythonModule (
          pkgs-mingw.rustPlatform.buildRustPackage {
            name = "nac3artiq";
            src = self;
            inherit cargoSha256;
            configurePhase =
              ''
              export PYO3_CONFIG_FILE=${pyo3-mingw-config}
              mkdir llvm-cfg
              ln -s ${pkgs-mingw.llvm_12.dev}/bin/llvm-config-native llvm-cfg/llvm-config
              export PATH=$PATH:`pwd`/llvm-cfg
              '';
            cargoBuildFlags = [ "--package" "nac3artiq" ];
            doCheck = false;
            installPhase =
              ''
              TARGET_DIR=$out/${pkgs.python3Packages.python.sitePackages}
              mkdir -p $TARGET_DIR
              #cp target/x86_64-unknown-linux-gnu/release/libnac3artiq.so $TARGET_DIR/nac3artiq.so
              ls target
              '';
            meta.platforms = ["x86_64-windows"];
          }
        );
      };

      devShell.x86_64-linux = pkgs.mkShell {
        name = "nac3-dev-shell";
        buildInputs = with pkgs; [
          llvm_12
          clang_12
          lld_12
          cargo
          cargo-insta
          rustc
          libffi
          libxml2
          clippy
          (python3.withPackages(ps: [ ps.numpy ]))
        ];
      };

      hydraJobs = {
        inherit (packages.x86_64-linux) nac3artiq;
      } // (pkgs.lib.foldr (a: b: {"${pkgs.lib.strings.getName a}" = a;} // b) {} devShell.x86_64-linux.buildInputs);
  };

  nixConfig = {
    binaryCachePublicKeys = ["nixbld.m-labs.hk-1:5aSRVA5b320xbNvu30tqxVPXpld73bhtOeH6uAjRyHc="];
    binaryCaches = ["https://nixbld.m-labs.hk" "https://cache.nixos.org"];
  };
}
