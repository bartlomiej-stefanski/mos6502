#pragma once

#include <cstdint>

#include "verilated.h"

inline VerilatedContext* contextp{};

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

using Addr = u16;

constexpr Addr NmiVector{0xFFFC};
constexpr Addr ResetVector{0xFFFC};
constexpr Addr InterruptVector{0xFFFC};

constexpr Addr StackStart{0x100};

template< typename T >
void reset_cpu(T& cpu)
{
  cpu.RESET = 1;
  cpu.CLK = 0;
  cpu.eval();

  cpu.RESET = 0;
  cpu.eval();
}

template< typename T >
void idle_cycles(T& cpu, u64 cycles)
{
  for (u64 i{}; i < cycles; i++) {
    cpu.CLK = 1;
    cpu.eval();
    cpu.CLK = 0;
    cpu.eval();
  }
}
