CXX = g++
VERILATOR = verilator

SOURCEDIR := $(abspath tests-verilator)
ALL_SOURCES := $(shell find $(SOURCEDIR) -name "*.cpp")

VERILOG_SOURCEDIR := $(abspath verilog/DebugTopLevel.topEntity)
BUILDDIR = $(abspath .build)

ROMDIR = $(abspath roms)

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
	mkdir -p $(ROMDIR)

compile-clash:
	@cabal run clash DebugTopLevel -- --verilog

all: compile-clash only-tests

only-tests: paths
	@mkdir -p $(BUILDDIR)
	$(VERILATOR) --top-module topEntity --Mdir $(BUILDDIR) \
	  $(VERILATOR_FLAGS) -I$(SOURCEDIR) --cc --build --exe \
	  $(VERILATOR_SOURCES) \
	  $(ALL_SOURCES)

test-prop:
	cabal test

vtest: only-tests
	$(BUILDDIR)/VtopEntity

test: test-prop vtest

full: compile-clash test


# VGA SIM SETTINGS

VGA_SOURCE_DIR = $(abspath VGASim)

VGA_INCLUDES = -I$(VGA_SOURCE_DIR)

vga-font: paths
	mkdir -p $(BUILDDIR)/vga-font
	$(CXX) $(CXXFLAGS) $(VGA_INCLUDES) -DGEN_FONT_FILE -o $(BUILDDIR)/vga-font/gen_font_file $(VGA_SOURCE_DIR)/Font.cpp

$(ROMDIR)/font8x8rom.bin: vga-font
	$(BUILDDIR)/vga-font/gen_font_file > $(ROMDIR)/font8x8rom.bin


clean:
	rm -rf $(BUILDDIR) $(VERILOG_SOURCEDIR)
