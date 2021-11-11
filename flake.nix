{
  description = "The third-generation ARTIQ compiler";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs }:
    let
      # We can't use overlays because llvm dependencies are handled internally in llvmPackages_xx
      pkgs-orig = import nixpkgs { system = "x86_64-linux"; };
      nixpkgs-patched = pkgs-orig.applyPatches {
        name = "nixpkgs";
        src = nixpkgs;
        patches = [ ./llvm-future-riscv-abi.diff ./llvm-restrict-targets.diff ];
      };
      pkgs = import nixpkgs-patched { system = "x86_64-linux"; };
    in rec {
      packages.x86_64-linux = {
        nac3artiq = pkgs.python3Packages.toPythonModule (
          pkgs.rustPlatform.buildRustPackage {
            name = "nac3artiq";
            src = self;
            cargoSha256 = "sha256-otKLhr58HYMjVXAof6AdObNpggPnvK6qOl7I+4LWIP8=";
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
      };
  };

  nixConfig = {
    binaryCachePublicKeys = ["nixbld.m-labs.hk-1:5aSRVA5b320xbNvu30tqxVPXpld73bhtOeH6uAjRyHc="];
    binaryCaches = ["https://nixbld.m-labs.hk" "https://cache.nixos.org"];
  };
}
