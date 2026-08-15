#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "CpuTest.hpp"
#include "MemoryArea.hpp"

constexpr Addr program_start{0x8090};

class ResetCheckTests : public CpuTest
{
protected:
  void SetUpMemory() override
  {
    CpuTest::SetUpMemory();

    memory_maps.insert({
      ResetVector,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "ResetVector",
        {MO(program_start)}
      ))
    });

    memory_maps.insert({
      program_start,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "Program Memory",
        std::vector< Instruction >{
          Instruction::nop(),
          Instruction::nop(),
          Instruction::nop()
        }
      ))
    });
  }
};

TEST_F(ResetCheckTests, InitialStateIsCorrect)
{
  for (auto i{0}; i < 50; i++) {
    cpu->RESET = 1;
    tick();

    EXPECT_EQ(cpu->REG_A, 0);
    EXPECT_EQ(cpu->REG_X, 0);
    EXPECT_EQ(cpu->REG_Y, 0);
    EXPECT_EQ(cpu->SP, 0xFF);
    EXPECT_EQ(cpu->PC, ResetVector);
    EXPECT_EQ(cpu->MEM_ADDR, 0);
    EXPECT_EQ(cpu->MEM_W, false);
  }
}

TEST_F(ResetCheckTests, InitialOperationsAreCorrect)
{
  cpu->RESET = 1;
  tick();
  cpu->RESET = 0;
  tick();

  {
    SCOPED_TRACE("Booting");
    expect_regs_change({.pc = ResetVector});
  }

  tick();

  {
    SCOPED_TRACE("Request LOW");
    expect_regs_change({.pc = ResetVector + 1});
    expect_bus_read(ResetVector);
  }

  tick();

  {
    SCOPED_TRACE("Request High");
    expect_regs_change({.pc = ResetVector + 2});
    expect_bus_read(ResetVector + 1);
  }

  tick();

  {
    SCOPED_TRACE("Read first program opcode");
    expect_regs_change({.pc = program_start + 1});
    expect_bus_read(program_start);
  }

  tick();

  {
    SCOPED_TRACE("Decode NOP opcode");
    expect_regs_change({.pc = program_start + 2});
    expect_bus_read(program_start + 1); // Pre-fetch next opcode
  }

  tick();

  {
    SCOPED_TRACE("Execute NOP: just ask for next opcode");
    expect_regs_change({.pc = program_start + 3});
    expect_bus_read(program_start + 2);
  }
}

TEST_F(ResetCheckTests, ResetBringsBackInitialState)
{
  reset();

  tick(4);

  EXPECT_EQ(cpu->PC, program_start + 2);

  reset();

  EXPECT_EQ(cpu->REG_A, 0);
  EXPECT_EQ(cpu->REG_X, 0);
  EXPECT_EQ(cpu->REG_Y, 0);
  EXPECT_EQ(cpu->SP, 0xFF);
  EXPECT_EQ(cpu->PC, ResetVector);
  EXPECT_EQ(cpu->MEM_ADDR, 0);
  EXPECT_EQ(cpu->MEM_W, false);
}
