{
  description = "The third-generation ARTIQ compiler";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      pkgs-mingw = import nixpkgs {
        system = "x86_64-linux";
        crossSystem = { config = "x86_64-w64-mingw32"; libc = "msvcrt"; };
        # work around https://github.com/NixOS/nixpkgs/issues/149593
        overlays = [
          (self: super: {
            openssh = super.openssh.overrideAttrs(oa: { doCheck = false; });
          })
        ];
      };
      msys2-python-tar = pkgs.fetchurl {
        url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-python-3.9.7-4-any.pkg.tar.zst";
        sha256 = "0iwlgbk4b457yn9djwqswid55xhyyi35qymz1lfh42xwdpxdm47c";
      };
      msys2-python = pkgs.stdenvNoCC.mkDerivation {
        name = "msys2-python";
        src = msys2-python-tar;
        buildInputs = [ pkgs.gnutar pkgs.zstd ];
        phases = [ "installPhase" ];
        installPhase =
          ''
          mkdir $out
          tar xf $src -C $out
          '';
      };
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
      packages.x86_64-linux = rec {
        llvm-nac3 = pkgs.callPackage "${self}/llvm" {};
        nac3artiq = pkgs.python3Packages.toPythonModule (
          pkgs.rustPlatform.buildRustPackage {
            name = "nac3artiq";
            src = self;
            cargoLock = {
              lockFile = ./Cargo.lock;
            };
            nativeBuildInputs = [ pkgs.python3 llvm-nac3 ];
            buildInputs = [ pkgs.python3 llvm-nac3 ];
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

      packages.x86_64-w64-mingw32 = rec {
        llvm-nac3 = pkgs-mingw.callPackage "${self}/llvm" { inherit (pkgs) llvmPackages_13; };
        nac3artiq = pkgs-mingw.python3Packages.toPythonModule (
          pkgs-mingw.rustPlatform.buildRustPackage {
            name = "nac3artiq";
            src = self;
            cargoLock = {
              lockFile = ./Cargo.lock;
            };
            nativeBuildInputs = [ pkgs.zip ];
            buildInputs = [ pkgs-mingw.zlib ];
            configurePhase =
              ''
              export PYO3_CONFIG_FILE=${pyo3-mingw-config}

              mkdir llvm-cfg
              cat << EOF > llvm-cfg/llvm-config
              #!${pkgs.bash}/bin/bash
              set -e
              # Gross hack to work around llvm-config asking for the wrong system libraries.
              exec ${llvm-nac3.dev}/bin/llvm-config-native \$@ | ${pkgs.gnused}/bin/sed s/-lrt\ -ldl\ -lpthread\ -lm//
              EOF
              chmod +x llvm-cfg/llvm-config
              export PATH=$PATH:`pwd`/llvm-cfg

              export CARGO_TARGET_X86_64_PC_WINDOWS_GNU_RUSTFLAGS="-C link-arg=-lz -C link-arg=-luuid -C link-arg=-lole32 -C link-arg=-lmcfgthread"
              '';
            cargoBuildFlags = [ "--package" "nac3artiq" ];
            doCheck = false;
            installPhase =
              ''
              mkdir -p $out $out/nix-support
              ln -s target/x86_64-pc-windows-gnu/release/nac3artiq.dll nac3artiq.pyd
              zip $out/nac3artiq.zip nac3artiq.pyd
              echo file binary-dist $out/nac3artiq.zip >> $out/nix-support/hydra-build-products
              '';
            dontFixup = true;
            meta.platforms = ["x86_64-windows"];
          }
        );
      };

      devShell.x86_64-linux = pkgs.mkShell {
        name = "nac3-dev-shell";
        buildInputs = with pkgs; [
          packages.x86_64-linux.llvm-nac3
          clang_13
          lld_13
          cargo
          cargo-insta
          rustc
          clippy
          (python3.withPackages(ps: [ ps.numpy ]))
        ];
      };

      hydraJobs = {
        inherit (packages.x86_64-linux) llvm-nac3 nac3artiq;
        llvm-nac3-mingw = packages.x86_64-w64-mingw32.llvm-nac3;
        nac3artiq-mingw = packages.x86_64-w64-mingw32.nac3artiq;
      };
  };

  nixConfig = {
    binaryCachePublicKeys = ["nixbld.m-labs.hk-1:5aSRVA5b320xbNvu30tqxVPXpld73bhtOeH6uAjRyHc="];
    binaryCaches = ["https://nixbld.m-labs.hk" "https://cache.nixos.org"];
  };
}
