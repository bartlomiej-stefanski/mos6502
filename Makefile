CXX = g++
VERILATOR = verilator

.PHONY: programs compile-clash all build-vtests vtest test-prop test clean
.DEFAULT_GOAL := all

# Compile programs with cc65.
programs:
	@make -C programs

# Compiles Clash CPU model to verilog code with debug outputs.
compile-clash: programs
	@cabal run clash DebugTopLevel -- --verilog
	@cabal run clash TopLevel -- --verilog
	@cabal run clash MemoryController -- --verilog

# Compiles Clash to verilog and compiles tests using verilator.
all: compile-clash build-vtests

# Re-compiles the verilator tests.
build-vtests: programs compile-clash
	@make -C tests-verilator

# Runs verilator tests.
vtest: compile-clash
	@make -C tests-verilator run

# Runs Haskell property tests.
test-prop:
	cabal test

# Runs all available tests.
test: test-prop vtest

clean:
	rm -rf verilog artifacts
	@make -C programs clean
	@make -C tests-verilator clean
	cabal clean
