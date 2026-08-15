CXX = g++
VERILATOR = verilator

SOURCEDIR := $(abspath tests-verilator)
ALL_SOURCES := $(shell find $(SOURCEDIR) -name "*.cpp")

VERILOG_SOURCEDIR := $(abspath verilog/DebugTopLevel.topEntity)
BUILDDIR = $(abspath .build)

GTEST_CFLAGS := $(shell pkg-config --cflags gtest)
GTEST_LIBS := $(shell pkg-config --libs gtest)

CXXFLAGS := -std=c++23 -Wall -Wextra -I$(SOURCEDIR) $(GTEST_CFLAGS)
LDFLAGS := $(GTEST_LIBS)

VERILATOR_IGNORE_CLASH_WARNINGS = -Wno-WIDTH -Wno-CASEINCOMPLETE -Wno-UNOPTFLAT
VERILATOR_FLAGS := $(VERILATOR_IGNORE_CLASH_WARNINGS) -j $(shell nproc) -CFLAGS "$(CXXFLAGS)" -LDFLAGS "$(LDFLAGS)"

VERILATOR_SOURCES = $(wildcard $(VERILOG_SOURCEDIR)/*.v)

.PHONY: run clean compile-clash test-prop vtest test full
.DEFAULT_GOAL := all

paths:
	mkdir -p $(BUILDDIR)

# Compiles Clash CPU model to verilog code with debug outputs.
compile-clash:
	@cabal run clash DebugTopLevel -- --verilog

# Compiles Clash to verilog and compiles tests using verilator.
all: compile-clash only-tests

# Re-compiles the verilator tests.
only-tests: paths
	@mkdir -p $(BUILDDIR)
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
