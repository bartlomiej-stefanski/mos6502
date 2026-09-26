#include <vector>

#include <gtest/gtest.h>

#include <VmemoryController.h>

#include "MemoryController.hpp"

class WriteTests : public MemoryController
{
protected:
  u8 reasonably_random(Addr a) {
    return static_cast< u64 >(a) * 1000007 + 177;
  }
};

TEST_F(WriteTests, ShouldWriteAndReadRam)
{
  reset();

  // Write whole RAM.
  for (Addr addr{RamStart}; addr <= RamEnd; addr++) {
    controller->CPU_DATA_OUT = reasonably_random(addr);
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = true;
    tick();

    // Written values should be immediatly readable.
    controller->CPU_DATA_W = false;
    tick();
    ASSERT_EQ(controller->CPU_DATA_IN, reasonably_random(addr));
  }

  controller->CPU_DATA_OUT = 0x0;

  // After all writes RAM should still have those values.
  for (Addr addr{RamStart}; addr <= RamEnd; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = false;
    tick();
    ASSERT_EQ(controller->CPU_DATA_IN, reasonably_random(addr));
  }

  // Reading RAM should not affect stored values.
  for (Addr addr{RamStart}; addr <= RamEnd; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = false;
    tick();
    ASSERT_EQ(controller->CPU_DATA_IN, reasonably_random(addr));
  }
}

TEST_F(WriteTests, ShouldWriteAndVgaBus)
{
  reset();

  // Write whole RAM.
  for (Addr addr{VgaStart}; addr <= VgaEnd; addr++) {
    controller->CPU_DATA_OUT = reasonably_random(addr);
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = true;
    tick();
  }

  // Simulate reading, VGA buffer is write-only so these should return 0.
  for (Addr addr{VgaStart}; addr <= VgaEnd; addr++) {
    controller->CPU_DATA_OUT = 0;
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = false;
    tick();
    ASSERT_EQ(controller->CPU_DATA_IN, 0);
  }

  // After all writes VGA bus should still have these data.
  for (Addr addr{VgaStart}; addr <= VgaEnd - 1; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = false;
    ASSERT_EQ(vga_bus.get_u8(addr - VgaStart), reasonably_random(addr));
  }
}

TEST_F(WriteTests, WritesDoNotAffectRom)
{
  reset();

  std::vector< u8 > rom_reads(RomEnd - RomStart + 1);

  // Read initial values.
  for (Addr addr{RomStart}; addr <= RomEnd && addr != 0; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = false;
    tick();
    rom_reads[addr - RomStart] = controller->CPU_DATA_IN;
  }

  // Simulate writes to ROM, they should not affect data.
  for (Addr addr{RomStart}; addr <= RomEnd && addr != 0; addr++) {
    controller->CPU_DATA_OUT = reasonably_random(addr);
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = true;
    tick();
  }

  // Re-read values should be the same as initial ones.
  for (Addr addr{RomStart}; addr <= RomEnd && addr != 0; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = false;
    tick();
    ASSERT_EQ(rom_reads[addr - RomStart], controller->CPU_DATA_IN);
  }
}

TEST_F(WriteTests, RamAndRomDoesNotAffectVGA)
{
  reset();

  for (Addr addr{RamStart}; addr <= RamEnd; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = true;
    controller->CPU_DATA_OUT = reasonably_random(addr);
    tick();

    ASSERT_FALSE(controller->VGA_DATA_W);
  }

  for (Addr addr{RomStart}; addr <= RomEnd && addr != 0; addr++) {
    controller->ADDR_QUERY = addr;
    controller->CPU_DATA_W = true;
    controller->CPU_DATA_OUT = reasonably_random(addr);
    tick();

    ASSERT_FALSE(controller->VGA_DATA_W);
  }
}
