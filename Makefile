CXX = g++
VERILATOR = verilator

SOURCEDIR := $(abspath tests-verilator)
ALL_SOURCES := $(shell find $(SOURCEDIR) -name "*.cpp")

ROM_BINARIES_DIR := $(abspath programs/.build/apps)

VERILOG_SOURCEDIR := $(abspath verilog/DebugTopLevel.topEntity)
BUILDDIR := $(abspath .build)
ARTIFACTDIR := $(abspath artifacts)

GTEST_CFLAGS := $(shell pkg-config --cflags gtest)
GTEST_LIBS := $(shell pkg-config --libs gtest)

LDFLAGS := $(GTEST_LIBS)
CXXFLAGS := -std=c++23 -Wall -Wextra -I$(SOURCEDIR) $(GTEST_CFLAGS) \
	-DROM_BINARIES_DIR='\"$(ROM_BINARIES_DIR)\"' \
	-DARTIFACT_DIR='\"$(ARTIFACTDIR)\"'

VERILATOR_IGNORE_CLASH_WARNINGS := -Wno-WIDTH -Wno-CASEINCOMPLETE -Wno-UNOPTFLAT
VERILATOR_FLAGS := $(VERILATOR_IGNORE_CLASH_WARNINGS) -j $(shell nproc) -CFLAGS "$(CXXFLAGS)" -LDFLAGS "$(LDFLAGS)"

VERILATOR_SOURCES := $(wildcard $(VERILOG_SOURCEDIR)/*.v)

.PHONY: run clean compile-clash test-prop vtest test full programs
.DEFAULT_GOAL := all

paths:
	mkdir -p $(BUILDDIR)

# Compile programs with cc65.
programs:
	@make -C programs all

# Compiles Clash CPU model to verilog code with debug outputs.
compile-clash: programs
	@cabal run clash DebugTopLevel -- --verilog
	@cabal run clash TopLevel -- --verilog
	@cabal run clash VgaDriver -- --verilog
	@cabal run clash MemoryController -- --verilog

# Compiles Clash to verilog and compiles tests using verilator.
all: compile-clash only-tests

# Re-compiles the verilator tests.
only-tests: paths programs
	@mkdir -p $(BUILDDIR) $(ARTIFACTDIR)
	$(VERILATOR) --top-module topEntity --Mdir $(BUILDDIR) \
	  $(VERILATOR_FLAGS) -I$(SOURCEDIR) --cc --build --exe \
	  $(VERILATOR_SOURCES) \
	  $(ALL_SOURCES)

# Runs Clash property tests.
test-prop:
	cabal test

# Runs verilator tests.
vtest: only-tests
	$(BUILDDIR)/VtopEntity

# Runs all available tests.
test: test-prop vtest

# Build everything and run all tests.
full: compile-clash test

clean:
	rm -rf $(BUILDDIR) $(VERILOG_SOURCEDIR)
	cabal clean
