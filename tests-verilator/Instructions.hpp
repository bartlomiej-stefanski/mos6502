#pragma once

#include <vector>

#include "Types.hpp"

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

enum class ZeroPageXOpcodes : u8
{
  STY = 0x94,
  STA = 0x95,

  LDY = 0xB4,
  LDA = 0xB5,

  CMP = 0xD5,

  ORA = 0x15,
  AND = 0x35,
  EOR = 0x55,

  ADC = 0x75,
  SBC = 0xF5,

  INC = 0xF6,
  DEC = 0xD6,

  ASL = 0x16,
  LSR = 0x56,
  ROL = 0x36,
  ROR = 0x76,
};

enum class ZeroPageYOpcodes : u8
{
  STX = 0x96,
  LDX = 0xB6,
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

enum class AbsoluteXOpcodes : u8
{
  ORA = 0x1D,
  AND = 0x3D,
  EOR = 0x5D,

  ADC = 0x7D,
  SBC = 0xFD,

  LDY = 0xBC,

  LDA = 0xBD,
  STA = 0x9D,

  CMP = 0xDD,

  ASL = 0x1E,
  LSR = 0x5E,
  ROL = 0x3E,
  ROR = 0x7E,

  DEC = 0xDE,
  INC = 0xFE,
};

enum class AbsoluteYOpcodes : u8
{
  ORA = 0x19,
  AND = 0x39,
  EOR = 0x59,

  ADC = 0x79,
  SBC = 0xF9,

  LDX = 0xBE,

  LDA = 0xB9,
  STA = 0x99,

  CMP = 0xBC,
};

enum class IndirectXOpcodes : u8
{
  ORA = 0x01,
  AND = 0x21,
  EOR = 0x41,

  ADC = 0x61,
  SBC = 0xE1,

  LDA = 0xA1,
  STA = 0x81,

  CMP = 0xC1,
};

enum class IndirectYOpcodes : u8
{
  ORA = 0x11,
  AND = 0x31,
  EOR = 0x51,

  ADC = 0x71,
  SBC = 0xF1,

  LDA = 0xB1,
  STA = 0x91,

  CMP = 0xD1,
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
  static Instruction zero_page(ZeroPageXOpcodes opcode, u8 offset);
  static Instruction zero_page(ZeroPageYOpcodes opcode, u8 offset);
  static Instruction absolute(AbsoluteOpcodes opcode, Addr address);
  static Instruction absolute(AbsoluteXOpcodes opcode, Addr address);
  static Instruction absolute(AbsoluteYOpcodes opcode, Addr address);
  static Instruction indirect(IndirectXOpcodes opcode, Addr address);
  static Instruction indirect(IndirectYOpcodes opcode, Addr address);

  static Instruction jumpAbsolute(Addr address);
  static Instruction jumpIndirect(Addr address);
  static Instruction jumpSoubroutine(Addr address);

private:
  std::vector< u8 > data;
};
