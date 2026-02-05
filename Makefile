CXX = g++
VERILATOR = verilator

VERILOG_TEST_SUITES = \
	SanityCheck \
	ResetCheck \
	ImmediateInstructions

SOURCEDIR := $(abspath tests-verilator)
VERILOG_SOURCEDIR := $(abspath verilog/DebugTopLevel.topEntity)
BUILDDIR = $(abspath .build)

GTEST_CFLAGS := $(shell pkg-config --cflags gtest)
GTEST_LIBS := $(shell pkg-config --libs gtest)

CXXFLAGS := -std=c++23 -Wall -Wextra -I$(SOURCEDIR) $(GTEST_CFLAGS)
LDFLAGS := $(GTEST_LIBS)

VERILATOR_IGNORE_CLASH_WARNINGS = -Wno-WIDTH -Wno-CASEINCOMPLETE -Wno-UNOPTFLAT
VERILATOR_FLAGS := $(VERILATOR_IGNORE_CLASH_WARNINGS) -j $(shell nproc) -CFLAGS "$(CXXFLAGS)" -LDFLAGS "$(LDFLAGS)"

COMMON_SRC = $(wildcard $(SOURCEDIR)/*.cpp)
VERILATOR_SOURCES = $(wildcard $(VERILOG_SOURCEDIR)/*.v)

.PHONY: run clean compile-clash test-prop vtest test full
.DEFAULT_GOAL := all

paths:
	mkdir -p $(BUILDDIR)

compile-clash:
	@cabal run clash DebugTopLevel -- --verilog

all: compile-clash only-tests

only-tests: paths
	@for test_suite in $(VERILOG_TEST_SUITES); do \
	  mkdir -p $(BUILDDIR)/$$test_suite ; \
    $(VERILATOR) --top-module topEntity --Mdir $(BUILDDIR)/$$test_suite \
      $(VERILATOR_FLAGS) -I$(SOURCEDIR) --cc --build --exe \
      $(VERILATOR_SOURCES) \
      $(SOURCEDIR)/"$$test_suite"/*.cpp \
      $(COMMON_SRC) ; \
  done

test-prop:
	cabal test

vtest: only-tests
	@for test_suite in $(VERILOG_TEST_SUITES); do \
	  echo -e "---------- RUNNING TEST SUITE $$test_suite ----------" ; \
    $(BUILDDIR)/$$test_suite/VtopEntity ; \
  done

test: test-prop vtest

full: compile-clash test

clean:
	rm -rf $(BUILDDIR) $(VERILOG_SOURCEDIR)
