#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Mos6502.hpp"
#include "Instructions.hpp"
#include "Bus/InstructionMemory.hpp"

class StackInstructions : public Mos6502
{
public:
  inline static constexpr Addr program_start{0x1000};
  inline static constexpr Addr program_a{program_start + 0x40};
  inline static constexpr Addr program_b{program_start - 0x40};

  StackInstructions() : Mos6502(Mos6502::JumpVector{.reset = program_start})
  {
    bus->insert_device(
      program_a,
      std::unique_ptr< BusDevice >(new InstructionMemory(
        "Program Memory",
        {
          Instruction::inner(InnerStateOpcodes::INX),
          Instruction::stack(StackOpcodes::RTS),
          Instruction::nop(),
        }
      ))
    );

    bus->insert_device(
      program_b,
      std::unique_ptr< BusDevice >(new InstructionMemory(
        "Program Memory",
        {
          Instruction::inner(InnerStateOpcodes::DEX),
          Instruction::stack(StackOpcodes::RTS),
          Instruction::nop(),
        }
      ))
    );
  }
};

TEST_F(StackInstructions, JumpUbroutineTest)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program",
      {
        Instruction::jumpSoubroutine(program_a),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();

  {
    SCOPED_TRACE("JSR push high PC");
    expect_regs_change({.pc = program_start + 2, .sp = *prev_state.sp - 1});
    expect_bus_write(*prev_state.sp + StackStart, (program_start + 2) >> 8);
  }

  tick();
  {
    SCOPED_TRACE("JSR push low PC");
    expect_regs_change({.sp = *prev_state.sp - 1});
    expect_bus_write(*prev_state.sp + StackStart, (program_start + 2) & 0xFF);
  }

  tick();
  {
    SCOPED_TRACE("JSR get high addr");
    expect_regs_change({.pc = program_start + 3});
    expect_bus_read(program_start + 2);
  }

  tick();
  {
    SCOPED_TRACE("JSR jump to subroutine");
    expect_regs_change({.pc = program_a + 1});
    expect_bus_read(program_a);
  }

  tick(); // Decode
  tick();
  {
    SCOPED_TRACE("program_a: shuold INX");
    expect_regs_change({.pc = program_a + 3, .x = 1});
  }
}


TEST_F(StackInstructions, JumpAndReturnSubroutineTest)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program",
      {
        Instruction::jumpSoubroutine(program_a),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();

  tick(3);

  ASSERT_EQ(cpu->PC, program_a + 1);

  tick(); // Decode
  tick();

  {
    SCOPED_TRACE("program_a: shuold INX");
    expect_regs_change({.pc = program_a + 3, .x = 1});
  }

  tick(); // Decode

  {
    SCOPED_TRACE("RTS pop low from stack");
    expect_regs_change({.sp = *prev_state.sp + 1});
    expect_bus_read(*prev_state.sp + StackStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("RTS pop high from stack");
    expect_regs_change({.sp = *prev_state.sp + 1});
    expect_bus_read(*prev_state.sp + StackStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("RTS update PC and increment it");
    expect_regs_change({.pc = program_start + 3});
  }

  tick();
  {
    SCOPED_TRACE("RTS fetch opcode after return");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 3);
  }

  tick(); // Decode
  tick();
  {
    SCOPED_TRACE("INY after return");
    expect_regs_change({.pc = NEXT_PC, .y = 1});
  }
}

TEST_F(StackInstructions, PushPullRegA)
{
  constexpr u8 rega_val{123};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, rega_val),
        Instruction::stack(StackOpcodes::PHA),
        Instruction::immediate(ImmediateOpcodes::LDA, rega_val - 20),
        Instruction::stack(StackOpcodes::PLA),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  tick(9);

  ASSERT_EQ(cpu->REG_A, rega_val);
  ASSERT_EQ(cpu->SP, 0xFF);
}

TEST_F(StackInstructions, PushPullStatus)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program",
      {
        Instruction::stack(StackOpcodes::PHP),
        Instruction::inner(InnerStateOpcodes::SED),
        Instruction::inner(InnerStateOpcodes::SEC),
        Instruction::stack(StackOpcodes::PLP),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  tick(8);

  ASSERT_EQ(cpu->DEC_AF, false);
  ASSERT_EQ(cpu->CARRY_AF, false);
  ASSERT_EQ(cpu->SP, 0xFF);
}
