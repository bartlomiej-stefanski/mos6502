#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "CpuTest.hpp"
#include "Instructions.hpp"

class StackInstructions : public CpuTest
{
protected:
  inline static constexpr Addr program_start{0x1000};
  inline static constexpr Addr program_a{program_start + 0x40};
  inline static constexpr Addr program_b{program_start - 0x40};

  void SetUpMemory() override
  {
    CpuTest::SetUpMemory();

    memory_maps.insert({
      ResetVector,
      MemoryLayer(
        "ResetVector",
        {MO((Addr)(program_start))}
      )
    });

    memory_maps.insert({
      program_a,
      MemoryLayer(
        "Program Memory",
        std::vector< Instruction >{
          Instruction::inner(InnerStateOpcodes::INX),
          Instruction::stack(StackOpcodes::RTS),
          Instruction::nop(),
        }
      )
    });

    memory_maps.insert({
      program_b,
      MemoryLayer(
        "Program Memory",
        std::vector< Instruction >{
          Instruction::inner(InnerStateOpcodes::DEX),
          Instruction::stack(StackOpcodes::RTS),
          Instruction::nop(),
        }
      )
    });
  }
};

TEST_F(StackInstructions, JumpUbroutineTest)
{
  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program",
      std::vector< Instruction >{
        Instruction::jumpSoubroutine(program_a),
        Instruction::nop(),
      }
    )
  });

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
  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program",
      std::vector< Instruction >{
        Instruction::jumpSoubroutine(program_a),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    )
  });

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
  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDA, rega_val),
        Instruction::stack(StackOpcodes::PHA),
        Instruction::immediate(ImmediateOpcodes::LDA, rega_val - 20),
        Instruction::stack(StackOpcodes::PLA),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
      }
    )
  });

  reset_to_entry();
  tick(9);

  ASSERT_EQ(cpu->REG_A, rega_val);
  ASSERT_EQ(cpu->SP, 0xFF);
}

TEST_F(StackInstructions, PushPullStatus)
{
  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program",
      std::vector< Instruction >{
        Instruction::stack(StackOpcodes::PHP),
        Instruction::inner(InnerStateOpcodes::SED),
        Instruction::inner(InnerStateOpcodes::SEC),
        Instruction::stack(StackOpcodes::PLP),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
        Instruction::nop(),
      }
    )
  });

  reset_to_entry();
  tick(8);

  ASSERT_EQ(cpu->DEC_AF, false);
  ASSERT_EQ(cpu->CARRY_AF, false);
  ASSERT_EQ(cpu->SP, 0xFF);
}
