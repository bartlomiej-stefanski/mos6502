#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"

constexpr Addr program_start{0x8090};

class ImmediateInstructions : public CpuTest
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

TEST_F(ImmediateInstructions, LoadRegisterTests)
{
  const u8 lda_data{0x42};
  const u8 ldx_data{0x21};
  const u8 ldy_data{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDA, lda_data),
        Instruction::immediate(ImmediateOpcodes::LDX, ldx_data),
        Instruction::immediate(ImmediateOpcodes::LDY, ldy_data),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 2); // LDX instruction

  {
    SCOPED_TRACE("LDA load immediate");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 1);
  }

  tick();

  {
    SCOPED_TRACE("LDA execute");
    expect_regs_change({.pc = NEXT_PC, .a = lda_data});
    expect_bus_read(program_start + 2);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("LDX load immediate");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 3);
  }

  tick();

  {
    SCOPED_TRACE("LDX execute");
    expect_regs_change({.pc = NEXT_PC, .x = ldx_data});
    expect_bus_read(program_start + 4);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("LDY load immediate");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 5);
  }

  tick();

  {
    SCOPED_TRACE("LDY execute");
    expect_regs_change({.pc = NEXT_PC, .y = ldy_data});
    expect_bus_read(program_start + 6);
  }
}

TEST_F(ImmediateInstructions, AdditionTests)
{
  const u8 lda_data{24};
  const u8 imm1{73};
  const u8 imm2{200};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDA, lda_data),
        Instruction::immediate(ImmediateOpcodes::ADC, imm1),
        Instruction::immediate(ImmediateOpcodes::ADC, imm2),
        Instruction::immediate(ImmediateOpcodes::ADC, 0),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  tick(3); // Execute LDA and decode next
  ASSERT_EQ(cpu->REG_A, lda_data);
  ASSERT_EQ(cpu->PC, program_start + 4); // Immediate of first ADC

  {
    SCOPED_TRACE("ADC load immediate (1)");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 3);
  }

  tick();

  {
    SCOPED_TRACE("ADC execute (1)");
    expect_regs_change({.pc = NEXT_PC, .a = lda_data + imm1});
    expect_flags_change({});
    expect_bus_read(program_start + 4);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("ADC load immediate (2)");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 5);
  }

  tick();

  {
    SCOPED_TRACE("ADC execute (2)");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + imm2});
    expect_flags_change({.carry = true});
    expect_bus_read(program_start + 6);
  }

  tick(); // Decoding

  tick(2);

  {
    SCOPED_TRACE("ADC execute (3)");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + 1});
    expect_flags_change({.carry = false});
  }
}

TEST_F(ImmediateInstructions, SubtractionTests)
{
  const u8 lda_data{88};
  const u8 imm1{80};
  const u8 imm2{8};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDA, lda_data),
        Instruction::immediate(ImmediateOpcodes::SBC, imm1),
        Instruction::immediate(ImmediateOpcodes::SBC, imm2),
        Instruction::immediate(ImmediateOpcodes::SBC, 0),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  tick(3); // Execute LDA and decode next
  ASSERT_EQ(cpu->REG_A, lda_data);
  ASSERT_EQ(cpu->PC, program_start + 4); // Immediate of first SBC

  {
    SCOPED_TRACE("SBC load immediate (1)");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 3);
  }

  tick();

  {
    SCOPED_TRACE("SBC execute (1)");
    expect_regs_change({.pc = NEXT_PC, .a = lda_data - imm1 - cpu->CARRY_AF});
    expect_flags_change({.carry = true});
    expect_bus_read(program_start + 4);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("SBC load immediate (2)");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 5);
  }

  tick();

  {
    SCOPED_TRACE("SBC execute (2)");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - imm2});
    expect_flags_change({.carry = false, .negative = true});
    expect_bus_read(program_start + 6);
  }

  tick(); // Decoding

  tick(2);

  {
    SCOPED_TRACE("SBC execute (3)");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - cpu->CARRY_AF});
    expect_flags_change({.carry = true});
  }
}

TEST_F(ImmediateInstructions, BinaryOpTest)
{
  const u8 lda_data{0b11001100};
  const u8 imm1{0b10101010};
  const u8 imm2{0b11110000};
  const u8 imm3{0b11111111};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDA, lda_data),
        Instruction::immediate(ImmediateOpcodes::ORA, imm1),
        Instruction::immediate(ImmediateOpcodes::AND, imm2),
        Instruction::immediate(ImmediateOpcodes::EOR, imm3),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  tick(3); // Execute LDA and decode next
  ASSERT_EQ(cpu->REG_A, lda_data);
  ASSERT_EQ(cpu->PC, program_start + 4); // Immediate of first ORA

  {
    SCOPED_TRACE("ORA load immediate (1)");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 3);
  }

  tick();

  {
    SCOPED_TRACE("ORA execute (1)");
    expect_regs_change({.pc = NEXT_PC, .a = lda_data | imm1});
    expect_flags_change({.negative = true});

    expect_bus_read(program_start + 4);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("AND load immediate (2)");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 5);
  }

  tick();

  {
    SCOPED_TRACE("AND execute (2)");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & imm2});
    expect_flags_change({.negative = true});
    expect_bus_read(program_start + 6);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("EOR load immediate (3)");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 7);
  }

  tick();

  {
    SCOPED_TRACE("EOR execute (3)");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ imm3});
    expect_flags_change({.negative = false});
    expect_bus_read(program_start + 8);
  }
}

TEST_F(ImmediateInstructions, CompareInstructions)
{
  const u8 lda_data{0x42};
  const u8 ldx_data{0x21};
  const u8 ldy_data{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDA, lda_data),
        Instruction::immediate(ImmediateOpcodes::LDX, ldx_data),
        Instruction::immediate(ImmediateOpcodes::LDY, ldy_data),
        Instruction::immediate(ImmediateOpcodes::CMP, lda_data - 5),
        Instruction::immediate(ImmediateOpcodes::CPX, ldx_data + 5),
        Instruction::immediate(ImmediateOpcodes::CPY, ldy_data),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  tick(8); // Load A, X, Y registers
  ASSERT_EQ(cpu->REG_A, lda_data);
  ASSERT_EQ(cpu->REG_X, ldx_data);
  ASSERT_EQ(cpu->REG_Y, ldy_data);
  ASSERT_EQ(cpu->PC, program_start + 7); // Second byte of

  tick();

  {
    SCOPED_TRACE("CMP load immediate");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 7);
  }

  tick();

  {
    SCOPED_TRACE("CMP execute");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({.carry = true});
    expect_bus_read(program_start + 8);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("CPX load immediate");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 9);
  }

  tick();

  {
    SCOPED_TRACE("CPX execute");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({.carry = false, .negative = true});
    expect_bus_read(program_start + 10);
  }

  tick(); // Decoding

  tick();

  {
    SCOPED_TRACE("CPY load immediate");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({});
    expect_bus_read(program_start + 11);
  }

  tick();

  {
    SCOPED_TRACE("CPY execute");
    expect_regs_change({.pc = NEXT_PC});
    expect_flags_change({.carry = true, .zero = true, .negative = false});
    expect_bus_read(program_start + 12);
  }
}
