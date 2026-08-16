# MOS 6502. in *Clash*.

## Dependencies

Nix development environment is provided in form of a `flake.nix` file.  
It uses unstable packages to get the newest verilator possible (should work with stable ones).

## Building

```sh
# Build the project.
cabal build

# Run interactive repl with Clashi.
cabal run clashi

# Generate HDL.
cabal run clash TopLevel -- --${hdl_language}

# Run tests.
cabal test

# Generate Debug Verilog
make compile-clash

# Run Verilator Tests.
make vtest
```

## Project Structure

- `src` -> CPU RTL model in Clash
- `tests` -> Prop-based tests in Haskell
- `programs` -> Applications for Mos6502 built using `cc65`
- `tests-verilator` -> Verilator tests for compiled cpu
  - `tests-verilator/Programs` -> tests for compiled programs
- `quartus` -> Project files generated for synthesis for DE-10 FPGA board
