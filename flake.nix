{
  description = "The third-generation ARTIQ compiler";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
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

      packages.x86_64-w64-mingw32 = import ./windows { inherit pkgs; };

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
          # MSYS2
          curl
          pacman
          fakeroot
          wineWowPackages.stable
        ];
      };

      hydraJobs = {
        inherit (packages.x86_64-linux) llvm-nac3 nac3artiq;
        llvm-nac3-msys2 = packages.x86_64-w64-mingw32.llvm-nac3;
        nac3artiq-msys2 = packages.x86_64-w64-mingw32.nac3artiq;
      };
  };

  nixConfig = {
    binaryCachePublicKeys = ["nixbld.m-labs.hk-1:5aSRVA5b320xbNvu30tqxVPXpld73bhtOeH6uAjRyHc="];
    binaryCaches = ["https://nixbld.m-labs.hk" "https://cache.nixos.org"];
  };
}
