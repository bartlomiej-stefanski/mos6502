#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "MemoryLayer.hpp"

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
  constexpr Addr buttonAddr{0x4003};

  constexpr Addr LedAddr{0x4000};
  constexpr Addr SegAddr{0x4001};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::INC, 0x50),

        Instruction::stack(StackOpcodes::PHP),
        Instruction::zero_page(ZeroPageOpcodes::LDA, 0x51),
        Instruction::stack(StackOpcodes::PLP),
        Instruction::immediate(ImmediateOpcodes::ADC, 0), // + Carry from INC
        Instruction::zero_page(ZeroPageOpcodes::STA, 0x51),

        Instruction::stack(StackOpcodes::PHP),
        Instruction::zero_page(ZeroPageOpcodes::LDA, 0x52),
        Instruction::stack(StackOpcodes::PLP),
        Instruction::immediate(ImmediateOpcodes::ADC, 0), // + Carry from INC
        // Instruction::zero_page(ZeroPageOpcodes::STA, 0x52),

        Instruction::absolute(AbsoluteOpcodes::STA, SegAddr),

        Instruction::absolute(AbsoluteOpcodes::CMP, SwitchAddr),
        Instruction::branch(BranchOpcodes::BNE, 2),
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

  memory_maps.insert({
    SegAddr,
    MemoryLayer(
      "First Page",
      std::vector< u8 >(4)
    )
  });

  reset_to_entry();

  tick(5 * 1000 * 1000); // 0.1s of execution at 50MHz

  const u8 m52 = get_memory(0x52, false);
  EXPECT_TRUE(m52 > 0);
}
