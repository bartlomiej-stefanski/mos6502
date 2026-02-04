# MOS 6502. in *Clash*.

## Dependencies

Nix development environment is provided in form of a `flake.nix` file.  
It uses unstable packages to get the newest verilator possible (should work with stable ones).

## Building

```sh
# Build the project
cabal build

# Run interactive repl with Clashi
cabal run clashi

# Generate HDL
cabal run clash TopLevel -- --${hdl_language}

# Run tests
cabal test

# Generate Debug Verilog
make compile-clash

# Run Verilator Tests
make vtest
```
