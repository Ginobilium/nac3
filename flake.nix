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
            outputs = [ "out" "runkernel" "standalone" ];
            src = self;
            cargoLock = { lockFile = ./Cargo.lock; };
            nativeBuildInputs = [ pkgs.python3 pkgs.llvmPackages_13.clang-unwrapped llvm-nac3 ];
            buildInputs = [ pkgs.python3 llvm-nac3 ];
            checkInputs = [ (pkgs.python3.withPackages(ps: [ ps.numpy ])) ];
            checkPhase =
              ''
              echo "Checking nac3standalone demos..."
              pushd nac3standalone/demo
              patchShebangs .
              ./check_demos.sh
              popd
              echo "Running Cargo tests..."
              cargoCheckHook
              '';
            installPhase =
              ''
              PYTHON_SITEPACKAGES=$out/${pkgs.python3Packages.python.sitePackages}
              mkdir -p $PYTHON_SITEPACKAGES
              cp target/x86_64-unknown-linux-gnu/release/libnac3artiq.so $PYTHON_SITEPACKAGES/nac3artiq.so

              mkdir -p $runkernel/bin
              cp target/x86_64-unknown-linux-gnu/release/runkernel $runkernel/bin

              mkdir -p $standalone/bin
              cp target/x86_64-unknown-linux-gnu/release/nac3standalone $standalone/bin
              '';
          }
        );
        python3-mimalloc = pkgs.python3 // rec {
          withMimalloc = pkgs.python3.buildEnv.override({ makeWrapperArgs = [ "--set LD_PRELOAD ${pkgs.mimalloc}/lib/libmimalloc.so" ]; });
          withPackages = f: let packages = f pkgs.python3.pkgs; in withMimalloc.override { extraLibs = packages; };
        };

        # LLVM PGO support
        llvm-nac3-instrumented = pkgs.callPackage "${self}/llvm" {
          stdenv = pkgs.llvmPackages_13.stdenv;
          extraCmakeFlags = [ "-DLLVM_BUILD_INSTRUMENTED=IR" ];
        };
        nac3artiq-instrumented = pkgs.python3Packages.toPythonModule (
          pkgs.rustPlatform.buildRustPackage {
            name = "nac3artiq-instrumented";
            src = self;
            cargoLock = { lockFile = ./Cargo.lock; };
            nativeBuildInputs = [ pkgs.python3 pkgs.llvmPackages_13.clang-unwrapped llvm-nac3-instrumented ];
            buildInputs = [ pkgs.python3 llvm-nac3-instrumented ];
            cargoBuildFlags = [ "--package" "nac3artiq" "--features" "init-llvm-profile" ];
            doCheck = false;
            configurePhase =
              ''
              export CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUSTFLAGS="-C link-arg=-L${pkgs.llvmPackages_13.compiler-rt}/lib/linux -C link-arg=-lclang_rt.profile-x86_64"
              '';
            installPhase =
              ''
              TARGET_DIR=$out/${pkgs.python3Packages.python.sitePackages}
              mkdir -p $TARGET_DIR
              cp target/x86_64-unknown-linux-gnu/release/libnac3artiq.so $TARGET_DIR/nac3artiq.so
              '';
          }
        );
        nac3artiq-profile = pkgs.stdenvNoCC.mkDerivation {
          name = "nac3artiq-profile";
          src = self;
          buildInputs = [ (python3-mimalloc.withPackages(ps: [ ps.numpy nac3artiq-instrumented ])) pkgs.lld_13 pkgs.llvmPackages_13.libllvm ];
          phases = [ "buildPhase" "installPhase" ];
          # TODO: get more representative code.
          buildPhase = "python $src/nac3artiq/demo/demo.py";
          installPhase =
            ''
            mkdir $out
            llvm-profdata merge -o $out/llvm.profdata /build/llvm/build/profiles/*
            '';
        };
        llvm-nac3-pgo = pkgs.callPackage "${self}/llvm" {
          stdenv = pkgs.llvmPackages_13.stdenv;
          extraCmakeFlags = [ "-DLLVM_PROFDATA_FILE=${nac3artiq-profile}/llvm.profdata" ];
        };
        nac3artiq-pgo = pkgs.python3Packages.toPythonModule (
          pkgs.rustPlatform.buildRustPackage {
            name = "nac3artiq-pgo";
            src = self;
            cargoLock = { lockFile = ./Cargo.lock; };
            nativeBuildInputs = [ pkgs.python3 pkgs.llvmPackages_13.clang-unwrapped llvm-nac3-pgo ];
            buildInputs = [ pkgs.python3 llvm-nac3-pgo ];
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
            cargoLock = { lockFile = ./Cargo.lock; };
            nativeBuildInputs = [ pkgs.llvmPackages_13.clang-unwrapped pkgs.llvmPackages_13.llvm pkgs.zip ];
            buildInputs = [ pkgs-mingw.zlib ];
            configurePhase =
              ''
              # Link libstdc++ statically. As usual with cargo, this is an adventure.
              cp --no-preserve=mode,ownership -R $CARGO_HOME/cargo-vendor-dir/llvm-sys-130.0.3/ llvm-sys-130.0.3
              substituteInPlace llvm-sys-130.0.3/build.rs --replace "cargo:rustc-link-lib=dylib=" "cargo:rustc-link-lib=static="
              substituteInPlace llvm-sys-130.0.3/build.rs --replace "fn main() {" "fn main() { println!(\"cargo:rustc-link-search=native=${pkgs-mingw.stdenv.cc.cc}/x86_64-w64-mingw32/lib\");"
              chmod 755 $CARGO_HOME/cargo-vendor-dir
              rm $CARGO_HOME/cargo-vendor-dir/llvm-sys-130.0.3
              mv llvm-sys-130.0.3 $CARGO_HOME/cargo-vendor-dir/llvm-sys-130.0.3

              export PYO3_CONFIG_FILE=${pyo3-mingw-config}

              mkdir llvm-cfg
              cat << EOF > llvm-cfg/llvm-config
              #!${pkgs.bash}/bin/bash
              set -e
              # Gross hack to work around llvm-config asking for the wrong system libraries.
              exec ${llvm-nac3.dev}/bin/llvm-config-native \$@ | ${pkgs.gnused}/bin/sed s/-lrt\ -ldl\ -lpthread\ -lm//
              EOF
              chmod +x llvm-cfg/llvm-config
              export PATH=`pwd`/llvm-cfg:$PATH

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
          # build dependencies
          packages.x86_64-linux.llvm-nac3
          llvmPackages_13.clang-unwrapped  # IRRT
          cargo
          rustc
          # runtime dependencies
          lld_13
          (packages.x86_64-linux.python3-mimalloc.withPackages(ps: [ ps.numpy ]))
          # development tools
          cargo-insta
          clippy
          rustfmt
        ];
      };

      hydraJobs = {
        inherit (packages.x86_64-linux) llvm-nac3 nac3artiq;
        llvm-nac3-mingw = packages.x86_64-w64-mingw32.llvm-nac3;
        nac3artiq-mingw = packages.x86_64-w64-mingw32.nac3artiq;
        mcfgthreads = pkgs-mingw.stdenvNoCC.mkDerivation {
          name = "mcfgthreads-hydra";
          phases = [ "installPhase" ];
          installPhase =
            ''
            mkdir -p $out $out/nix-support
            ln -s ${pkgs-mingw.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/
            echo file binary-dist $out/mcfgthread-12.dll >> $out/nix-support/hydra-build-products
            '';
        };
      };
  };

  nixConfig = {
    binaryCachePublicKeys = ["nixbld.m-labs.hk-1:5aSRVA5b320xbNvu30tqxVPXpld73bhtOeH6uAjRyHc="];
    binaryCaches = ["https://nixbld.m-labs.hk" "https://cache.nixos.org"];
  };
}
