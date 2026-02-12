#pragma once

#include <string>
#include <vector>
#include <variant>
#include <stdexcept>

#include "Types.hpp"
#include "Instructions.hpp"

constexpr Addr NmiVector{0xFFFC};
constexpr Addr ResetVector{0xFFFC};
constexpr Addr InterruptVector{0xFFFC};

constexpr Addr StackStart{0x100};

using MemoryOccupant = std::variant< Addr, u8, Instruction >;
using MO = MemoryOccupant;

struct UnmappedMemory : std::runtime_error
{
  UnmappedMemory(std::string error_message)
    : std::runtime_error(error_message)
  {
  }
};

struct MemoryLayer : std::vector< u8 >
{
  MemoryLayer(const std::string name, std::vector< MemoryOccupant >&& data);
  MemoryLayer(const std::string name, std::vector< u8 >&& data);
  MemoryLayer(const std::string name, std::vector< Instruction >&& data);

  std::string name;
};
