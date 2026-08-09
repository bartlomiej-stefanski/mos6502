#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "CpuTest.hpp"
#include "Instructions.hpp"

constexpr Addr program_start{0x8090};

class Counter : public CpuTest
{
protected:
  void SetUpMemory() override
  {
    CpuTest::SetUpMemory();

    memory_maps.insert({
      ResetVector,
      MemoryLayer(
        "ResetVector",
        {MO(program_start)}
      )
    });
  }
};

TEST_F(Counter, ShouldCountOnMemory)
{
  constexpr Addr SwitchAddr{0x4002};
  constexpr Addr NumAddr{0x50};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::INC, NumAddr),

        // Second byte of the numberS.
        Instruction::stack(StackOpcodes::PHP),
        Instruction::zero_page(ZeroPageOpcodes::LDA, NumAddr + 1),
        Instruction::stack(StackOpcodes::PLP),
        Instruction::immediate(ImmediateOpcodes::ADC, 0), // + Carry from INC
        Instruction::zero_page(ZeroPageOpcodes::STA, NumAddr + 1),

        // Third byte of the number.
        Instruction::stack(StackOpcodes::PHP),
        Instruction::zero_page(ZeroPageOpcodes::LDA, 0x52),
        Instruction::stack(StackOpcodes::PLP),
        Instruction::immediate(ImmediateOpcodes::ADC, 0), // + Carry from INC

        // Reset the number if above one set by switches.
        Instruction::absolute(AbsoluteOpcodes::CMP, SwitchAddr),
        Instruction::branch(BranchOpcodes::BNE, 2), // Skips the follwing LDA if true.
          Instruction::immediate(ImmediateOpcodes::LDA, 0),

        Instruction::zero_page(ZeroPageOpcodes::STA, 0x52),

        Instruction::jumpAbsolute(program_start)
      }
    )
  });

  memory_maps.insert({
    0x01,
    MemoryLayer(
      "First Page",
      std::vector< u8 >(1024)
    )
  });

  memory_maps.insert({
    SwitchAddr,
    MemoryLayer(
      "Switches",
      std::vector< u8 >{0xF0}
    )
  });

  reset_to_entry();

  tick(5 * 1000 * 1000); // 0.1s of execution at 50MHz

  const u8 m52 = get_memory(0x52, false);

  // Smoke-test: We should iterate over 2^16 num of times.
  EXPECT_TRUE(m52 > 0);
}
