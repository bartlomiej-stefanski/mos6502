#pragma once

#include <vector>

#include "Common.hpp"

enum class ImmediateOpcodes : u8
{
  LDA = 0xA9,
  LDX = 0xA2,
  LDY = 0xA0,

  CMP = 0xC9,
  CPY = 0xC0,
  CPX = 0xE0,

  ORA = 0x09,
  AND = 0x29,
  EOR = 0x49,

  ADC = 0x69,
  SBC = 0xE9,
};

class Instruction
{
  Instruction() = default;

public:
  std::vector< u8 > to_bytes() const;

  static Instruction nop();

  static Instruction immediate(ImmediateOpcodes opcode, u8 immediate);

private:
  std::vector< u8 > data;
};
