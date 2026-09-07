#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Mos6502.hpp"
#include "Instructions.hpp"
#include "BitHelper.hpp"
#include "Bus/InstructionMemory.hpp"

constexpr Addr program_start{0x8090};

class InnerStateInstructions : public Mos6502
{
public:
  inline static constexpr u8 lda_data{0x42};
  inline static constexpr u8 ldx_data{0x21};
  inline static constexpr u8 ldy_data{0x37};

  inline static constexpr u8 lda_add{90};

  InnerStateInstructions() : Mos6502(Mos6502::JumpVector{.reset = program_start - 6})
  {
    bus->insert_device(
      program_start - 6,
      std::unique_ptr< BusDevice >(new InstructionMemory(
        "Program Memory",
        {
          Instruction::immediate(ImmediateOpcodes::LDA, lda_data),
          Instruction::immediate(ImmediateOpcodes::LDX, ldx_data),
          Instruction::immediate(ImmediateOpcodes::LDY, ldy_data),
        }
      ))
    );
  }

  void LoadRegisters()
  {
    reset_to_entry();
    tick(5); // Load A, X, Y registers.

    {
      SCOPED_TRACE("Registers loaded");
      ASSERT_EQ(cpu->REG_A, lda_data);
      ASSERT_EQ(cpu->REG_X, ldx_data);
      ASSERT_EQ(cpu->REG_Y, ldy_data);
      ASSERT_EQ(cpu->PC, program_start + 2); // Read and decoded first instruction.
      ASSERT_EQ(cpu->MEM_ADDR, program_start + 1);
    }
  }
};

TEST_F(InnerStateInstructions, CarryFlagOperations)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::inner(InnerStateOpcodes::SEC),
        Instruction::inner(InnerStateOpcodes::CLC),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick();
  {
    SCOPED_TRACE("SEC");
    expect_flags_change({.carry = true});
    expect_bus_read(program_start + 2);
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("CLC");
    expect_flags_change({.carry = false});
    expect_bus_read(program_start + 3);
  }
}

TEST_F(InnerStateInstructions, InterruptFlagOperations)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::inner(InnerStateOpcodes::SEI),
        Instruction::inner(InnerStateOpcodes::CLI),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick();
  {
    SCOPED_TRACE("SEI");
    expect_flags_change({.interrupt_disable = true});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("CLI");
    expect_flags_change({.interrupt_disable = false});
  }
}

TEST_F(InnerStateInstructions, DecimalFlagOperations)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::inner(InnerStateOpcodes::SED),
        Instruction::inner(InnerStateOpcodes::CLD),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick();
  {
    SCOPED_TRACE("SED");
    expect_flags_change({.decimal_mode = true});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("CLD");
    expect_flags_change({.decimal_mode = false});
  }
}

TEST_F(InnerStateInstructions, OverflowFlagOperations)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::immediate(ImmediateOpcodes::ADC, lda_add),
        Instruction::inner(InnerStateOpcodes::CLV),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick(2);
  {
    SCOPED_TRACE("ADC");
    ASSERT_EQ(cpu->REG_A, lda_data + lda_add);
    expect_flags_change({.overflow = true, .negative = true});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("CLV");
    expect_flags_change({.overflow = false});
  }
}

TEST_F(InnerStateInstructions, IncrementOperators)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::inner(InnerStateOpcodes::INX),
        Instruction::inner(InnerStateOpcodes::DEY),
        Instruction::inner(InnerStateOpcodes::DEX),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick();
  {
    SCOPED_TRACE("INY");
    expect_regs_change({.pc = NEXT_PC, .y = *prev_state.y + 1});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("INX");
    expect_regs_change({.pc = NEXT_PC, .x = *prev_state.x + 1});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("DEY");
    expect_regs_change({.pc = NEXT_PC, .y = *prev_state.y - 1});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("DEX");
    expect_regs_change({.pc = NEXT_PC, .x = *prev_state.x - 1});
  }
}

TEST_F(InnerStateInstructions, TransferOperators)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::inner(InnerStateOpcodes::TYA),
        Instruction::inner(InnerStateOpcodes::TXS),
        Instruction::inner(InnerStateOpcodes::TAX),
        Instruction::inner(InnerStateOpcodes::TSX),
        Instruction::inner(InnerStateOpcodes::TXA),
        Instruction::inner(InnerStateOpcodes::TAY),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick();
  {
    SCOPED_TRACE("TYA");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.y});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("TXS");
    expect_regs_change({.pc = NEXT_PC, .sp = *prev_state.x});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("TAX");
    expect_regs_change({.pc = NEXT_PC, .x = *prev_state.a});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("TSX");
    expect_regs_change({.pc = NEXT_PC, .x = *prev_state.sp});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("TXA");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.x});
  }

  tick(); // Decode and execute
  {
    SCOPED_TRACE("TAY");
    expect_regs_change({.pc = NEXT_PC, .y = *prev_state.a});
  }
}

TEST_F(InnerStateInstructions, ShiftAndRotate)
{
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::inner(InnerStateOpcodes::ASL),
        Instruction::inner(InnerStateOpcodes::LSR),
        Instruction::inner(InnerStateOpcodes::ROL),
        Instruction::inner(InnerStateOpcodes::ROR),
        Instruction::nop(),
      }
    ))
  );

  LoadRegisters();

  tick();
  {
    SCOPED_TRACE("ASL");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a << 1});
    expect_flags_change({.negative = true});
  }

  tick();
  {
    SCOPED_TRACE("LSR");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a >> 1});
    expect_flags_change({.negative = false});
  }

  tick();
  {
    SCOPED_TRACE("ROL");
    expect_regs_change({.pc = NEXT_PC, .a = rol(*prev_state.a, *prev_flags.carry)});
    expect_flags_change({.negative = true});
  }

  tick();
  {
    SCOPED_TRACE("ROR");
    expect_regs_change({.pc = NEXT_PC, .a = ror(*prev_state.a, *prev_flags.carry)});
    expect_flags_change({.negative = false});
  }
}
