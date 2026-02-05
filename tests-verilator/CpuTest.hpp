#pragma once

#include <stdexcept>
#include <optional>
#include <map>
#include <variant>

#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "Instructions.hpp"

using MemoryOccupant = std::variant< Addr, u8, Instruction >;
using MO = MemoryOccupant;

struct UnmappedMemory : std::runtime_error
{
  UnmappedMemory(std::string error_message)
    : std::runtime_error(error_message)
  {
  }
};

class CpuTest : public ::testing::Test
{
protected:
  struct MemoryLayer : std::vector< u8 >
  {
    MemoryLayer(const std::string name, std::vector< MemoryOccupant >&& data);
    MemoryLayer(const std::string name, std::vector< u8 >&& data);
    MemoryLayer(const std::string name, std::vector< Instruction >&& data);

    std::string name;
  };

  VtopEntity* cpu{nullptr};

  std::map< Addr, MemoryLayer > memory_maps;

  virtual void SetUpMemory();
  void SetUp() override;
  void TearDown() override;

  void reset();
  static constexpr u64 ResetEntryCycles{5};
  void reset_to_entry() { reset(); tick(ResetEntryCycles); }

  u8& get_memory(Addr addr, bool is_write);

  void tick();
  void tick(u64 n);

  struct RegsState
  {
    std::optional< Addr > pc{};
    std::optional< u8 > a{};
    std::optional< u8 > x{};
    std::optional< u8 > y{};
    std::optional< u8 > sp{};
  };

  RegsState prev_state{
    .pc = ResetVector,
    .a = 0,
    .x = 0,
    .y = 0,
    .sp = 0xFF
  };

#define NEXT_PC (*prev_state.pc + 1)

  struct CpuFlagState
  {
    std::optional< bool > carry{};
    std::optional< bool > zero{};
    std::optional< bool > interrupt_disable{};
    std::optional< bool > decimal_mode{};
    std::optional< bool > break_command{};
    std::optional< bool > overflow{};
    std::optional< bool > negative{};
  };

  CpuFlagState prev_flags{
    .carry = false,
    .zero = false,
    .interrupt_disable = false,
    .decimal_mode = false,
    .break_command = false,
    .overflow = false,
    .negative = false
  };

  void expect_regs_change(RegsState expected);
  void expect_flags_change(CpuFlagState expected);

  void expect_bus_read(Addr bus_addr);
  void expect_bus_write(Addr bus_addr, u8 data);
};
