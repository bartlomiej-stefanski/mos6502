#pragma once

#include <fstream>
#include <memory>
#include <optional>

#include <gtest/gtest.h>

#include <VtopEntity.h>
#include <string>

#include "Instructions.hpp"
#include "Bus/MemoryBus.hpp"


class Mos6502 : public ::testing::Test
{
public:
  static constexpr Addr NmiVector{0xFFFA};
  static constexpr Addr ResetVector{0xFFFC};
  static constexpr Addr InterruptVector{0xFFFE};

  static constexpr Addr StackStart{0x100};

  struct JumpVector {
    Addr nmi{};
    Addr reset{};
    Addr interrupt{};
  };

  ~Mos6502();

protected:
  static constexpr u64 ResetEntryCycles{5};

  VtopEntity* cpu{nullptr};
  std::shared_ptr< MemoryBus< Addr > > bus{nullptr};

  std::optional< std::ofstream > log_output;

  Mos6502();
  Mos6502(JumpVector jump_vector);

  void SetUp() override;
  void TearDown() override;

  void reset();
  void reset_to_entry() { reset(); tick(ResetEntryCycles); }

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

private:
  void setup_memory();

  std::string get_flag_text();
  std::string get_opcode_text(const OpCodeInfo& opcode_info, Addr next_pc);
};
