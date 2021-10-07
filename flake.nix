{
  description = "The third-generation ARTIQ compiler";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in {
      packages.x86_64-linux = {
        nac3artiq = pkgs.python3Packages.toPythonModule (
          pkgs.rustPlatform.buildRustPackage {
            name = "nac3artiq";
            src = self;
            cargoSha256 = "0cml3irmc72f42dqmka9w4l3k397b2rnns768vm132mh2zyv85lp";
            nativeBuildInputs = [ pkgs.python3 pkgs.llvm_11 ];
            buildInputs = [ pkgs.python3 pkgs.libffi pkgs.libxml2 pkgs.llvm_11 ];
            cargoBuildFlags = [ "--package" "nac3artiq" ];
            cargoTestFlags = [ "--package" "nac3core" "--package" "nac3artiq" ];
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
          llvm_11
          clang_11
          lld_11
          cargo
          rustc
          libffi
          libxml2
          clippy
          (python3.withPackages(ps: [ ps.numpy ]))
        ];
      };
  };
}
