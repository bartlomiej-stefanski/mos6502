CXX ?= g++
VERILATOR ?= verilator

ROOT ?= $(abspath .)

GTEST_CFLAGS := $(shell pkg-config --cflags gtest)
GTEST_LIBS := $(shell pkg-config --libs gtest)

BUILD_DIR := $(ROOT)/.build
ROM_BINARIES_DIR := $(ROOT)/../programs/.build/apps
ARTIFACT_DIR := $(ROOT)/artifacts

LDFLAGS := $(GTEST_LIBS)
CXXFLAGS := -std=c++23 -Wall -Wextra -I$(ROOT) $(GTEST_CFLAGS) \
	-DROM_BINARIES_DIR='\"$(ROM_BINARIES_DIR)\"' \
	-DARTIFACT_DIR='\"$(ARTIFACT_DIR)\"'

COMMON_SOURCES := $(wildcard $(ROOT)/*.cpp) \
	$(wildcard $(ROOT)/Bus/*.cpp)

VERILATOR_IGNORE_CLASH_WARNINGS := -Wno-WIDTH -Wno-CASEINCOMPLETE -Wno-UNOPTFLAT

define VTestSuite_Template
$(1)_C = $$(shell find $(ROOT)/$(1) -name '*.cpp')
$(1)_VERILOG_SOURCEDIR := $$(wildcard $(ROOT)/../verilog/$(1).*)
$(1)_VERILOG_MODULE := $$(patsubst $(ROOT)/../verilog/$(1).%,%,$$($(1)_VERILOG_SOURCEDIR))
$(1)_VERILATOR_SOURCES := $$(wildcard $$($(1)_VERILOG_SOURCEDIR)/*.v)

$(1)_VERILATOR_FLAGS := $(VERILATOR_IGNORE_CLASH_WARNINGS) \
  -j $(shell nproc) -CFLAGS "-I$(ROOT)/$(1) $(CXXFLAGS)" -LDFLAGS "$(LDFLAGS)"

.PHONY: $(1)

$(1):
	@mkdir -p $(BUILD_DIR)/$(1)
	$(VERILATOR) --top-module $$($(1)_VERILOG_MODULE) --Mdir $(BUILD_DIR)/$(1) \
	  $$($(1)_VERILATOR_FLAGS) -I$(ROOT) -I$(ROOT)/$(1) --cc --build --exe \
	  $$($(1)_VERILATOR_SOURCES) \
	  $(COMMON_SOURCES)  $$($(1)_C)
	@echo "Created test suite '$(1)' in '$(BUILD_DIR)/$(1)'"

$(1)_run: $(1)
	$(BUILD_DIR)/$(1)/V$$($(1)_VERILOG_MODULE)
endef

ALL_CORES := DebugTopLevel
RUN_TARGETS := $(foreach app,$(ALL_CORES),$(app)_run)

$(foreach app,$(ALL_CORES),$(eval $(call VTestSuite_Template,$(app))))

.PHONY: all run clean
.DEFAULT_GOAL := all

all: $(ALL_CORES)

run: $(RUN_TARGETS)

clean:
	rm -rf $(BUILD_DIR)
