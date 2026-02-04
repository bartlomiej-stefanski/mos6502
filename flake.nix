{
  description = "Clash FPGA Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let

        pkgs = import nixpkgs {
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

        # Packages only required on CI
        ciPackages = with pkgs; [
          haskellEnv
          cabal-install
          ormolu

          gnumake
          ninja
          gcc
          pkg-config
          gtest

          verilator
        ];

        # Packages for development shells
        devPackages = with pkgs; [
          # Heavy FPGA Tools
          quartus-prime-lite
          openocd

          # GUI & Visualization
          gtkwave

          # Language Servers & Helpers
          haskell-language-server
          bear

          # C++ Dev
          clang-tools
          gdb
          clang-manpages

          # Python
          python313
          python313Packages.python-lsp-server

          # Utilities
          hexedit
          socat
          minicom
        ];

      in {
        devShells = {
          ci = pkgs.mkShell {
            packages = ciPackages;
          };

          default = pkgs.mkShell {
            packages = ciPackages ++ devPackages;

            shellHook = ''
              echo "Welcome to your CLASH Hardware development environment!"
              ghc --version
              clash --version
              verilator --version
            '';
          };
        };
      }
    );
}
