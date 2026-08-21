#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Bus/ArrayMemory.hpp"
#include "Mos6502.hpp"
#include "Instructions.hpp"
#include "Bus/InstructionMemory.hpp"

class BranchInstructions : public Mos6502
{
public:
  inline static constexpr Addr program_start{0x1000};
  inline static constexpr Addr program_a{program_start + 0x40};
  inline static constexpr Addr program_b{program_start - 0x40};

  BranchInstructions() : Mos6502(Mos6502::JumpVector{.reset = program_start})
  {
    bus->insert_device(
      program_a,
      std::unique_ptr< BusDevice >(new InstructionMemory(
        "Program Memory",
        {
          Instruction::inner(InnerStateOpcodes::INX),
          Instruction::nop(),
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
          Instruction::nop(),
          Instruction::nop(),
        }
      ))
    );
  }
};

#define TAKE_BRANCH_TEST() \
  { \
    SCOPED_TRACE("Calculating Branch"); \
    expect_regs_change({.pc = program_a}); \
  } \
  \
  tick(); \
  { \
    SCOPED_TRACE("Taking Branching"); \
    expect_regs_change({.pc = NEXT_PC}); \
    expect_bus_read(program_a); \
  } \
  tick(2); \
  { \
    SCOPED_TRACE("INX after taken branch"); \
    expect_regs_change({.pc = NEXT_PC, .x = 1}); \
  }

#define TAKE_BACKWARD_BRANCH_TEST() \
  { \
    SCOPED_TRACE("Calculating Branch"); \
    expect_regs_change({.pc = program_b}); \
  } \
  \
  tick(); \
  { \
    SCOPED_TRACE("Taking Branching"); \
    expect_regs_change({.pc = NEXT_PC}); \
    expect_bus_read(program_b); \
  } \
  tick(2); \
  { \
    SCOPED_TRACE("INX after taken branch"); \
    expect_regs_change({.pc = NEXT_PC, .x = -1}); \
  }

#define SKIP_BRANCH_TEST() \
  { \
    SCOPED_TRACE("Calculating Branch"); \
    expect_regs_change({}); \
  } \
  \
  tick(); \
  { \
    SCOPED_TRACE("Skip Branching"); \
    expect_regs_change({.pc = NEXT_PC}); \
    expect_bus_read(program_start + 4); \
  } \
  tick(2); \
  { \
    SCOPED_TRACE("INY after missed branch"); \
    expect_regs_change({.pc = NEXT_PC, .y = 1}); \
  }

TEST_F(BranchInstructions, TakeBPL)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BPL, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->NEG_AF, false); // Negative clear to take the branch.

  TAKE_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBackwardBPL)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BPL, program_b - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->NEG_AF, false); // Negative clear to take the branch.

  TAKE_BACKWARD_BRANCH_TEST()
}

TEST_F(BranchInstructions, SkipBPL)
{
  constexpr u8 reg_a{0xFF};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BPL, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->NEG_AF, true); // Negative active to skip the branch.

  SKIP_BRANCH_TEST()
}

TEST_F(BranchInstructions, SkipBMI)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BMI, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->NEG_AF, false); // Negative clear to skip the branch.

  SKIP_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBMI)
{
  constexpr u8 reg_a{0xFF};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BMI, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->NEG_AF, true); // Negative active to take the branch.

  TAKE_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBackwardBMI)
{
  constexpr u8 reg_a{0xFF};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BMI, program_b - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->NEG_AF, true); // Negative active to take the branch.

  TAKE_BACKWARD_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBNE)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BNE, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->ZERO_AF, false); // Zero clear to take the branch.

  TAKE_BRANCH_TEST()
}

TEST_F(BranchInstructions, SkipBNE)
{
  constexpr u8 reg_a{0x0};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BNE, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->ZERO_AF, true); // Zero active to skip the branch.

  SKIP_BRANCH_TEST()
}

TEST_F(BranchInstructions, SkipBEQ)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BEQ, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->ZERO_AF, false); // Zero clear to skip the branch.

  SKIP_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBEQ)
{
  constexpr u8 reg_a{0x0};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BEQ, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->ZERO_AF, true); // Zero active to take the branch.

  TAKE_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBVC)
{
  constexpr u8 reg_a{0x0};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BVC, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->OVF_AF, false); // Overflow clear to take the branch.

  TAKE_BRANCH_TEST()
}

TEST_F(BranchInstructions, SkipBVS)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BVS, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->ZERO_AF, false); // Overflow clear to skip the branch.

  SKIP_BRANCH_TEST()
}

TEST_F(BranchInstructions, TakeBCC)
{
  constexpr u8 reg_a{0x0};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BCC, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->CARRY_AF, false); // Carry clear to take the branch.

  TAKE_BRANCH_TEST()
}

TEST_F(BranchInstructions, SkipBCS)
{
  constexpr u8 reg_a{0x10};
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::LDA, reg_a),
        Instruction::branch(BranchOpcodes::BCS, program_a - (program_start + 4)),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(2); // LDA
  ASSERT_EQ(cpu->REG_A, reg_a);
  ASSERT_EQ(cpu->CARRY_AF, false); // Carry clear to skip the branch.

  SKIP_BRANCH_TEST()
}

TEST_F(BranchInstructions, JumpAbsolute)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::jumpAbsolute(program_a),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick(); // Read second part of immediate
  {
    SCOPED_TRACE("Jump to AbsoluteValue");
    expect_regs_change({.pc = program_a + 1});
    ASSERT_EQ(cpu->MEM_ADDR, program_a);
  }

  tick(2);
  {
    SCOPED_TRACE("INX after jump");
    expect_regs_change({.pc = NEXT_PC, .x = 1});
  }
}

TEST_F(BranchInstructions, JumpIndirect)
{
  constexpr Addr JumpTable{0x2000};
  bus->insert_device(
    JumpTable,
    std::unique_ptr< BusDevice >(new ArrayMemory< Addr >(
      "Jump Table",
      {program_b}
    ))
  );

  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::jumpIndirect(JumpTable),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::nop(),
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3);

  tick();
  {
    SCOPED_TRACE("Read first part of jump address");
    expect_regs_change({});
    ASSERT_EQ(cpu->MEM_ADDR, JumpTable);
  }

  tick();
  {
    SCOPED_TRACE("Read second part of jump address");
    expect_regs_change({});
    ASSERT_EQ(cpu->MEM_ADDR, JumpTable + 1);
    ASSERT_EQ(cpu->LATCH, program_b & 0xFF);
  }

  tick();
  {
    SCOPED_TRACE("Read second part of jump address");
    expect_regs_change({.pc = program_b + 1});
    ASSERT_EQ(cpu->MEM_ADDR, program_b);
  }

  tick(2);
  {
    SCOPED_TRACE("DEX after jump");
    expect_regs_change({.pc = NEXT_PC, .x = -1});
  }
}
