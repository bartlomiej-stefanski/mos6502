#pragma once

#include "verilated.h"

#include "Types.hpp"

inline VerilatedContext* contextp{};

constexpr Addr NmiVector{0xFFFC};
constexpr Addr ResetVector{0xFFFC};
constexpr Addr InterruptVector{0xFFFC};

constexpr Addr StackStart{0x100};
