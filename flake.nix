{
  description = "Clash FPGA Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs =  import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        haskellEnv = pkgs.haskellPackages.ghcWithPackages (p: with p; [
          # Clash Libraries
          clash-ghc
          clash-prelude
          clash-lib
          # Testing Libraries
          doctest
          doctest-parallel
          QuickCheck
          tasty
          tasty-th
          hedgehog
          tasty-hedgehog
          clash-prelude-hedgehog
        ]);
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # Build utilites
            gnumake
            cmake
            ninja
            bear
            pkg-config

            # Compilers
            gcc
            clang-manpages
            clang-tools

            # C++ Libraries
            gtest

            # Debugger and tools
            gdb

            # Simulation and Synthesis
            iverilog
            verilator
            gtkwave

            # Quartus
            quartus-prime-lite
            openocd

            # Haskell and Clash stack
            haskellEnv
            cabal-install
            haskell-language-server

            # python13
            python313
            python313Packages.python-lsp-server

            # General random utilites
            hexedit
            socat
            minicom
          ];

          shellHook = ''
            echo "Welcome to your CLASH Hardware development environment!"
            ghc --version
            clash --version
            verilator --version
          '';
        };
      }
    );
}
