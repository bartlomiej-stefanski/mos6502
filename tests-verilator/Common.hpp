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

constexpr u16 NmiVector{0xFFFC};
constexpr u16 ResetVector{0xFFFC};
constexpr u16 InterruptVector{0xFFFC};

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
