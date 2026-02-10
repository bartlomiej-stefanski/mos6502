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

enum class InnerStateOpcodes : u8
{
  CLC = 0x18,
  SEC = 0x38,
  CLI = 0x58,
  SEI = 0x78,
  CLV = 0xB8,
  CLD = 0xD8,
  SED = 0xF8,

  DEY = 0x88,
  INY = 0xC8,
  DEX = 0xCA,
  INX = 0xE8,

  TYA = 0x98,
  TAY = 0xA8,
  TXA = 0x8A,
  TAX = 0xAA,
  TXS = 0x9A,
  TSX = 0xBA,
};

enum class BranchOpcodes : u8
{
  BPL = 0x10,
  BMI = 0x30,
  BVC = 0x50,
  BVS = 0x70,
  BCC = 0x90,
  BCS = 0xB0,
  BNE = 0xD0,
  BEQ = 0xF0,
};

class Instruction
{
  Instruction() = default;

public:
  std::vector< u8 > to_bytes() const;

  static Instruction nop();

  static Instruction immediate(ImmediateOpcodes opcode, u8 immediate);
  static Instruction inner(InnerStateOpcodes opcode);
  static Instruction branch(BranchOpcodes opcode, i8 offset);
  static Instruction jumpAbsolute(Addr address);
  static Instruction jumpIndirect(Addr address);

private:
  std::vector< u8 > data;
};
