#pragma once

#include <fstream>
#include <optional>

#include <gtest/gtest.h>

#include <VmemoryController.h>

#include "Types.hpp"
#include "Bus/ArrayMemory.hpp"

class MemoryController : public ::testing::Test
{
public:

  static constexpr Addr RamStart{0};
  static constexpr Addr RamEnd{0x8000 - 1};

  static constexpr Addr VgaStart{0x8000};
  static constexpr Addr VgaEnd{0xA000 - 1};

  static constexpr Addr RomStart{0xE000};
  static constexpr Addr RomEnd{0xFFFF};

  ~MemoryController();

protected:
  static constexpr u64 ResetEntryCycles{5};

  VmemoryController* controller{nullptr};
  ArrayMemory< u8 > vga_bus = ArrayMemory< u8 >("vga_bus", std::vector< u8 >(VgaEnd - VgaStart + 1));

  std::optional< std::ofstream > log_output;

  MemoryController();

  void SetUp() override;
  void TearDown() override;

  void reset();

  void tick();
  void tick(u64 n);

private:
  void setup_memory();
};
