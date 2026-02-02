#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"

TEST(ResetCheck, InitialStateIsCorrect)
{
  VtopEntity cpu;
  cpu.ENABLE = 1;
  cpu.RESET = 0;
  cpu.CLK = 0;
  cpu.eval();
  cpu.RESET = 1;
  cpu.CLK = 1;
  cpu.eval();

  EXPECT_EQ(cpu.REG_A, 0);
  EXPECT_EQ(cpu.REG_X, 0);
  EXPECT_EQ(cpu.REG_Y, 0);
  EXPECT_EQ(cpu.SP, 0xFF);
  EXPECT_EQ(cpu.PC, ResetVector + 1);
  EXPECT_EQ(cpu.MEM_ADDR, ResetVector);
  EXPECT_EQ(cpu.MEM_W, false);
}
