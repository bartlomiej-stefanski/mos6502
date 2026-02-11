#pragma once

#include <vector>

#include "Common.hpp"

enum class StackOpcodes : u8
{
  BRK = 0x00,
  RTI = 0x40,

  RTS = 0x60,

  PHP = 0x08,
  PLP = 0x28,
  PHA = 0x48,
  PLA = 0x68,
};

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

enum class ZeroPageOpcodes : u8
{
  BIT = 0x24,

  STY = 0x84,
  STX = 0x86,
  STA = 0x85,

  LDY = 0xA4,
  LDX = 0xA6,
  LDA = 0xA5,

  CPY = 0xC4,
  CPX = 0xE4,
  CMP = 0xC5,

  ORA = 0x05,
  AND = 0x25,
  EOR = 0x45,

  ADC = 0x65,
  SBC = 0xE5,

  INC = 0xE6,
  DEC = 0xC6,

  ASL = 0x06,
  LSR = 0x46,
  ROL = 0x26,
  ROR = 0x66,
};

enum class AbsoluteOpcodes : u8
{
  BIT = 0x2C,

  STA = 0x8D,
  STX = 0x8E,
  STY = 0x8C,

  LDY = 0xAC,
  LDX = 0xAE,
  LDA = 0xAD,

  CPY = 0xCC,
  CPX = 0xEC,
  CMP = 0xCD,

  ORA = 0x0D,
  AND = 0x2D,
  EOR = 0x4D,

  ADC = 0x6D,
  SBC = 0xED,

  INC = 0xEE,
  DEC = 0xCE,

  ASL = 0x0E,
  LSR = 0x4E,
  ROL = 0x2E,
  ROR = 0x6E,
};

class Instruction
{
  Instruction() = default;

public:
  std::vector< u8 > to_bytes() const;

  static Instruction nop();

  static Instruction stack(StackOpcodes opcode);
  static Instruction immediate(ImmediateOpcodes opcode, u8 immediate);
  static Instruction inner(InnerStateOpcodes opcode);
  static Instruction branch(BranchOpcodes opcode, i8 offset);
  static Instruction zero_page(ZeroPageOpcodes opcode, u8 offset);
  static Instruction absolute(AbsoluteOpcodes opcode, Addr address);

  static Instruction jumpAbsolute(Addr address);
  static Instruction jumpIndirect(Addr address);
  static Instruction jumpSoubroutine(Addr address);

private:
  std::vector< u8 > data;
};
