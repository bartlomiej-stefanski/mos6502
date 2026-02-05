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

Instruction Instruction::immediate(ImmediateOpcodes opcode, u8 immediate)
{
  Instruction lda;
  lda.data.emplace_back((u8)opcode);
  lda.data.emplace_back(immediate);
  return lda;
}
