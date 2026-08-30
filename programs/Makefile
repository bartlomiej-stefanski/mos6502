ROOT ?= .
CC := cc65
AS := ca65
LD := ld65

CC65_TARGET := none

AFLAGS := --target $(CC65_TARGET)
CFLAGS := $(AFLAGS) -O
LDFLAGS := -C linker.cfg

BUILDDIR := $(ROOT)/.build
COMMONDIR := $(ROOT)/common
APPSDIR := $(ROOT)/apps

COMMON_C := $(wildcard $(COMMONDIR)/*.c)
COMMON_S := $(wildcard $(COMMONDIR)/*.s)

COMMON_OBJ := $(patsubst $(ROOT)/%.c,$(BUILDDIR)/%.o,$(COMMON_C)) \
  $(patsubst $(ROOT)/%.s,$(BUILDDIR)/%.o,$(COMMON_S))

APP_NAMES := $(notdir $(wildcard $(APPSDIR)/*))

.PHONY: all clean $(BUILDDIR)


$(BUILDDIR)/%.o: $(ROOT)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $< -o $(@:.o=.s)
	$(AS) $(AFLAGS) $(@:.o=.s) -o $@

$(BUILDDIR)/%.o: $(ROOT)/%.s
	@mkdir -p $(dir $@)
	$(AS) $(AFLAGS) $< -o $@


define APP_Template
$(1)_C = $$(wildcard $(APPSDIR)/$(1)/*.c)
$(1)_S = $$(wildcard $(APPSDIR)/$(1)/*.s)

$(1)_OBJ = $$(patsubst $(APPSDIR)/$(1)/%.c,$$(BUILDDIR)/apps/$(1)/%.o,$$($(1)_C)) \
  $$(patsubst $(APPSDIR)/$(1)/%.s,$$(BUILDDIR)/apps/$(1)/%.o,$$($(1)_S))

.PHONY: $(1)

$(1): $$(BUILDDIR)/apps/$(1)/$(1).bin

$$(BUILDDIR)/apps/$(1)/$(1).bin: $$($(1)_OBJ) $$(COMMON_OBJ)
	@mkdir -p $$(dir $$@)
	$(LD) $(LDFLAGS) -m $$(BUILDDIR)/apps/$(1)/$(1).map -o $$@ $$($(1)_OBJ) $$(COMMON_OBJ) none.lib
	@xxd -b -c 1 $$@ | awk '{print $$$$2}' > $$@.txt
	@echo "-> Application $(1) saved as $$@"
endef


$(foreach app,$(APP_NAMES),$(eval $(call APP_Template,$(app))))

clean:
	rm -rf $(BUILDDIR)

all: $(APP_NAMES)
