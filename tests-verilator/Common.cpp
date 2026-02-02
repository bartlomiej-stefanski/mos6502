#include <gtest/gtest.h>

#include "verilated.h"

#include "Common.hpp"

int main(int argc, char** argv)
{
  testing::InitGoogleTest(&argc, argv);

  contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  int result{RUN_ALL_TESTS()};

  delete contextp;

  return result;
}

TEST(SanityCheck, AmISsane)
{
  EXPECT_TRUE(true);
}
