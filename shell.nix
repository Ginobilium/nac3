let
  pkgs = import <nixpkgs> { };
in
  pkgs.stdenv.mkDerivation {
    name = "nac3-env";
    buildInputs = with pkgs; [
      llvm_10 clang_10 cargo rustc libffi libxml2 clippy
    ];
    LLVM_SYS_100_PREFIX="${pkgs.llvm_10}";
  }
