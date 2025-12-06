# MOS 6502. in *Clash*.

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
```
