let
  pkgs = import <nixpkgs> { };
in
  pkgs.stdenv.mkDerivation {
    name = "nac3-env";
    buildInputs = with pkgs; [
      llvm_11
      clang_11
      lld_11
      cargo
      rustc
      libffi
      libxml2
      clippy
      (python3.withPackages(ps: [ps.numpy]))
    ];
  }
