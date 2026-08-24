#include <stdexcept>

#include "Instructions.hpp"
#include "Types.hpp"

std::vector< u8 > Instruction::to_bytes() const
{
  return data;
}

Instruction Instruction::nop()
{
  Instruction nop;
  nop.data.emplace_back(0xEA);
  return nop;
}

Instruction Instruction::stack(StackOpcodes opcode)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  return ins;
}

Instruction Instruction::immediate(ImmediateOpcodes opcode, u8 immediate)
{
  Instruction lda;
  lda.data.emplace_back((u8)opcode);
  lda.data.emplace_back(immediate);
  return lda;
}

Instruction Instruction::inner(InnerStateOpcodes opcode)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  return ins;
}

Instruction Instruction::branch(BranchOpcodes opcode, i8 offset)
{
  Instruction branch;
  branch.data.emplace_back((u8)opcode);
  branch.data.emplace_back((u8)offset);
  return branch;
}

Instruction Instruction::zero_page(ZeroPageOpcodes opcode, u8 offset)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back(offset);
  return ins;
}

Instruction Instruction::zero_page(ZeroPageXOpcodes opcode, u8 offset)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back(offset);
  return ins;
}

Instruction Instruction::zero_page(ZeroPageYOpcodes opcode, u8 offset)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back(offset);
  return ins;
}

Instruction Instruction::absolute(AbsoluteOpcodes opcode, Addr address)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back((u8)(address & 0xFF));
  ins.data.emplace_back((u8)((address >> 8) & 0xFF));
  return ins;
}

Instruction Instruction::absolute(AbsoluteXOpcodes opcode, Addr address)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back((u8)(address & 0xFF));
  ins.data.emplace_back((u8)((address >> 8) & 0xFF));
  return ins;
}

Instruction Instruction::absolute(AbsoluteYOpcodes opcode, Addr address)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back((u8)(address & 0xFF));
  ins.data.emplace_back((u8)((address >> 8) & 0xFF));
  return ins;
}

Instruction Instruction::indirect(IndirectXOpcodes opcode, Addr address)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back((u8)(address & 0xFF));
  if (address >> 8)
  {
    throw std::runtime_error("Indirect X instruction address must point to the zero-page.");
  }

  return ins;
}

Instruction Instruction::indirect(IndirectYOpcodes opcode, Addr address)
{
  Instruction ins;
  ins.data.emplace_back((u8)opcode);
  ins.data.emplace_back((u8)(address & 0xFF));
  if (address >> 8)
  {
    throw std::runtime_error("Indirect Y instruction address must point to the zero-page.");
  }

  return ins;
}

Instruction Instruction::jumpAbsolute(Addr address)
{
  Instruction jpm;
  jpm.data.emplace_back(0x4C);
  jpm.data.emplace_back((u8)(address & 0xFF));
  jpm.data.emplace_back((u8)((address >> 8) & 0xFF));
  return jpm;
}

Instruction Instruction::jumpIndirect(Addr address)
{
  Instruction jpm;
  jpm.data.emplace_back(0x6C);
  jpm.data.emplace_back((u8)(address & 0xFF));
  jpm.data.emplace_back((u8)((address >> 8) & 0xFF));
  return jpm;
}

Instruction Instruction::jumpSoubroutine(Addr address)
{
  Instruction jpm;
  jpm.data.emplace_back(0x20);
  jpm.data.emplace_back((u8)(address & 0xFF));
  jpm.data.emplace_back((u8)((address >> 8) & 0xFF));
  return jpm;
}

static const OpCodeInfo op_table[256] = {
  // 0x00
  {"BRK",IMP,1}, {"ORA",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"ORA",ZP,2},   {"ASL",ZP,2},   {"???",IMP,1},
  {"PHP",IMP,1}, {"ORA",IMM,2},  {"ASL",ACC,1}, {"???",IMP,1}, {"???",IMP,1}, {"ORA",ABS,3},  {"ASL",ABS,3},  {"???",IMP,1},
  // 0x10
  {"BPL",REL,2}, {"ORA",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"ORA",ZPX,2},  {"ASL",ZPX,2},  {"???",IMP,1},
  {"CLC",IMP,1}, {"ORA",ABSY,3}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"ORA",ABSX,3}, {"ASL",ABSX,3}, {"???",IMP,1},
  // 0x20
  {"JSR",ABS,3}, {"AND",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"BIT",ZP,2},  {"AND",ZP,2},   {"ROL",ZP,2},   {"???",IMP,1},
  {"PLP",IMP,1}, {"AND",IMM,2},  {"ROL",ACC,1}, {"???",IMP,1}, {"BIT",ABS,3}, {"AND",ABS,3},  {"ROL",ABS,3},  {"???",IMP,1},
  // 0x30
  {"BMI",REL,2}, {"AND",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"AND",ZPX,2},  {"ROL",ZPX,2},  {"???",IMP,1},
  {"SEC",IMP,1}, {"AND",ABSY,3}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"AND",ABSX,3}, {"ROL",ABSX,3}, {"???",IMP,1},
  // 0x40
  {"RTI",IMP,1}, {"EOR",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"EOR",ZP,2},   {"LSR",ZP,2},   {"???",IMP,1},
  {"PHA",IMP,1}, {"EOR",IMM,2},  {"LSR",ACC,1}, {"???",IMP,1}, {"JMP",ABS,3}, {"EOR",ABS,3},  {"LSR",ABS,3},  {"???",IMP,1},
  // 0x50
  {"BVC",REL,2}, {"EOR",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"EOR",ZPX,2},  {"LSR",ZPX,2},  {"???",IMP,1},
  {"CLI",IMP,1}, {"EOR",ABSY,3}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"EOR",ABSX,3}, {"LSR",ABSX,3}, {"???",IMP,1},
  // 0x60
  {"RTS",IMP,1}, {"ADC",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"ADC",ZP,2},   {"ROR",ZP,2},   {"???",IMP,1},
  {"PLA",IMP,1}, {"ADC",IMM,2},  {"ROR",ACC,1}, {"???",IMP,1}, {"JMP",IND,3}, {"ADC",ABS,3},  {"ROR",ABS,3},  {"???",IMP,1},
  // 0x70
  {"BVS",REL,2}, {"ADC",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"ADC",ZPX,2},  {"ROR",ZPX,2},  {"???",IMP,1},
  {"SEI",IMP,1}, {"ADC",ABSY,3}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"ADC",ABSX,3}, {"ROR",ABSX,3}, {"???",IMP,1},
  // 0x80
  {"???",IMP,1}, {"STA",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"STY",ZP,2},  {"STA",ZP,2},   {"STX",ZP,2},   {"???",IMP,1},
  {"DEY",IMP,1}, {"???",IMP,1},  {"TXA",IMP,1}, {"???",IMP,1}, {"STY",ABS,3}, {"STA",ABS,3},  {"STX",ABS,3},  {"???",IMP,1},
  // 0x90
  {"BCC",REL,2}, {"STA",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"STY",ZPX,2}, {"STA",ZPX,2},  {"STX",ZPY,2},  {"???",IMP,1},
  {"TYA",IMP,1}, {"STA",ABSY,3}, {"TXS",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"STA",ABSX,3}, {"???",IMP,1},  {"???",IMP,1},
  // 0xA0
  {"LDY",IMM,2}, {"LDA",INDX,2}, {"LDX",IMM,2}, {"???",IMP,1}, {"LDY",ZP,2},  {"LDA",ZP,2},   {"LDX",ZP,2},   {"???",IMP,1},
  {"TAY",IMP,1}, {"LDA",IMM,2},  {"TAX",IMP,1}, {"???",IMP,1}, {"LDY",ABS,3}, {"LDA",ABS,3},  {"LDX",ABS,3},  {"???",IMP,1},
  // 0xB0
  {"BCS",REL,2}, {"LDA",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"LDY",ZPX,2}, {"LDA",ZPX,2},  {"LDX",ZPY,2},  {"???",IMP,1},
  {"CLV",IMP,1}, {"LDA",ABSY,3}, {"TSX",IMP,1}, {"???",IMP,1}, {"LDY",ABSX,3},{"LDA",ABSX,3}, {"LDX",ABSY,3}, {"???",IMP,1},
  // 0xC0
  {"CPY",IMM,2}, {"CMP",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"CPY",ZP,2},  {"CMP",ZP,2},   {"DEC",ZP,2},   {"???",IMP,1},
  {"INY",IMP,1}, {"CMP",IMM,2},  {"DEX",IMP,1}, {"???",IMP,1}, {"CPY",ABS,3}, {"CMP",ABS,3},  {"DEC",ABS,3},  {"???",IMP,1},
  // 0xD0
  {"BNE",REL,2}, {"CMP",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"CMP",ZPX,2},  {"DEC",ZPX,2},  {"???",IMP,1},
  {"CLD",IMP,1}, {"CMP",ABSY,3}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"CMP",ABSX,3}, {"DEC",ABSX,3}, {"???",IMP,1},
  // 0xE0
  {"CPX",IMM,2}, {"SBC",INDX,2}, {"???",IMP,1}, {"???",IMP,1}, {"CPX",ZP,2},  {"SBC",ZP,2},   {"INC",ZP,2},   {"???",IMP,1},
  {"INX",IMP,1}, {"SBC",IMM,2},  {"NOP",IMP,1}, {"???",IMP,1}, {"CPX",ABS,3}, {"SBC",ABS,3},  {"INC",ABS,3},  {"???",IMP,1},
  // 0xF0
  {"BEQ",REL,2}, {"SBC",INDY,2}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"SBC",ZPX,2},  {"INC",ZPX,2},  {"???",IMP,1},
  {"SED",IMP,1}, {"SBC",ABSY,3}, {"???",IMP,1}, {"???",IMP,1}, {"???",IMP,1}, {"SBC",ABSX,3}, {"INC",ABSX,3}, {"???",IMP,1}
};

OpCodeInfo Instruction::get_opcode_info(u8 opcode)
{
  return op_table[opcode];
}
