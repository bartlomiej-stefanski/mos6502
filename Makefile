CXX = g++
VERILATOR = verilator

SOURCEDIR := $(abspath tests-verilator)
COMMON_SOURCES := $(wildcard $(SOURCEDIR)/*.cpp)

DEBUG_CPU_SOURCES := $(shell find $(SOURCEDIR)/DebugCpuTests -name "*.cpp")

VERILOG_SOURCEDIR := $(abspath verilog)
BUILDDIR = $(abspath .build)

ROMDIR = $(abspath roms)

GTEST_CFLAGS := $(shell pkg-config --cflags gtest)
GTEST_LIBS := $(shell pkg-config --libs gtest)

CXXFLAGS := -std=c++23 -Wall -Wextra -I$(SOURCEDIR) -I$(SOURCEDIR)/DebugCpuTests $(GTEST_CFLAGS)
LDFLAGS := $(GTEST_LIBS)

VERILATOR_IGNORE_CLASH_WARNINGS = -Wno-WIDTH -Wno-CASEINCOMPLETE -Wno-UNOPTFLAT
VERILATOR_FLAGS := $(VERILATOR_IGNORE_CLASH_WARNINGS) -j $(shell nproc) -CFLAGS "$(CXXFLAGS)" -LDFLAGS "$(LDFLAGS)"

DEBUG_VERILATOR_SOURCES = $(wildcard $(VERILOG_SOURCEDIR)/DebugTopLevel.topEntity/*.v)

.PHONY: run clean compile-clash test-prop vtest test full
.DEFAULT_GOAL := all

paths:
	mkdir -p $(BUILDDIR)
	mkdir -p $(ROMDIR)

compile-clash-debug:
	@cabal run clash DebugTopLevel -- --verilog

all: compile-clash-debug only-tests

only-tests: paths
	@mkdir -p $(BUILDDIR)
	$(VERILATOR) --top-module topEntity --Mdir $(BUILDDIR) \
	  $(VERILATOR_FLAGS) -I$(SOURCEDIR) --cc --build --exe \
	  $(DEBUG_VERILATOR_SOURCES) \
	  $(DEBUG_CPU_SOURCES) $(COMMON_SOURCES)

test-prop:
	cabal test

vtest: only-tests
	$(BUILDDIR)/VtopEntity

test: test-prop vtest

full: compile-clash-debug test


# VGA SIM SETTINGS

ROMGEN_SOURCE_DIR = $(abspath RomGen)
ROMGEN_SOURCES = $(wildcard $(ROMGEN_SOURCE_DIR)/*.cpp) $(COMMON_SOURCES)

ROMGEN_INCLUDES = -I$(ROMGEN_SOURCE_DIR)

vga-font: paths
	mkdir -p $(BUILDDIR)/vga-font
	$(CXX) $(CXXFLAGS) $(ROMGEN_INCLUDES) -o $(BUILDDIR)/vga-font/vga-font $(ROMGEN_SOURCE_DIR)/Font/Font.cpp $(ROMGEN_SOURCES)

$(ROMDIR)/font8x8rom.bin: vga-font
	$(BUILDDIR)/vga-font/vga-font > $(ROMDIR)/font8x8rom.bin

CounterSample: paths
	mkdir -p $(BUILDDIR)/CounterSample
	$(CXX) $(CXXFLAGS) $(ROMGEN_INCLUDES) -o $(BUILDDIR)/CounterSample/CounterSample $(ROMGEN_SOURCE_DIR)/CounterSample/CounterSample.cpp $(ROMGEN_SOURCES)


$(ROMDIR)/CounterSample.bin: CounterSample
	$(BUILDDIR)/CounterSample/CounterSample > $(ROMDIR)/CounterSample.bin

compile-clash-counter: $(ROMDIR)/font8x8rom.bin $(ROMDIR)/CounterSample.bin
	@ln -sf $(ROMDIR)/CounterSample.bin $(ROMDIR)/code.bin
	@cabal run clash TopLevel -- --verilog

# Build VASM for MOS6502
VASM_SOURCE_DIR = $(abspath vasm)
VASM_BUILD_FLAGS = SYNTAX=oldstyle CPU=6502

VASM_BIN = $(VASM_SOURCE_DIR)/obj/vasm6502_oldstyle
VASM_FLAGS = -Fbin -dotdir

$(VASM_BIN): paths
	@make -C $(VASM_SOURCE_DIR) $(VASM_BUILD_FLAGS)

clean:
	rm -rf $(BUILDDIR) $(VERILOG_SOURCEDIR)
