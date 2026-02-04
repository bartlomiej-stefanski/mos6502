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

Instruction Instruction::lda_immediate(u8 immediate)
{
  Instruction lda;
  lda.data.emplace_back(0xA9);
  lda.data.emplace_back(immediate);
  return lda;
}
