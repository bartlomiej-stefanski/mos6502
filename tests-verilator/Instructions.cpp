#include <stdexcept>

#include "Instructions.hpp"

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
