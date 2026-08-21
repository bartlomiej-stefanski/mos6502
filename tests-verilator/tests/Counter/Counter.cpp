#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Mos6502.hpp"
#include "Instructions.hpp"
#include "Bus/ArrayMemory.hpp"
#include "Bus/InstructionMemory.hpp"

constexpr Addr program_start{0x8090};

class Counter : public Mos6502
{
public:
  Counter() : Mos6502(Mos6502::JumpVector{.reset = program_start})
  {
  }
};

TEST_F(Counter, ShouldCountOnMemory)
{
  constexpr Addr SwitchAddr{0x4002};
  constexpr Addr NumAddr{0x50};

  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
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
    ))
  );

  bus->insert_device(
    0x01,
    std::unique_ptr< BusDevice >(new ArrayMemory(
      "First Page",
      std::vector< u8 >(1024)
    ))
  );

  bus->insert_device(
    SwitchAddr,
    std::unique_ptr< BusDevice >(new ArrayMemory(
      "Switches",
      std::vector< u8 >{0xF0}
    ))
  );

  reset_to_entry();

  tick(5 * 1000 * 1000); // 0.1s of execution at 50MHz

  const u8 m52 = bus->get< u8 >(0x52);

  // Smoke-test: We should iterate over 2^16 num of times.
  EXPECT_TRUE(m52 > 0);
}
