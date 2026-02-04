#pragma once

#include <vector>

#include "Common.hpp"

class Instruction
{
  Instruction() = default;

public:
  std::vector< u8 > to_bytes() const;

  static Instruction nop();

  static Instruction lda_immediate(u8 immediate);

private:
  std::vector< u8 > data;
};
