#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "BitHelper.hpp"

constexpr Addr program_start{0x8090};

class ZeroPageInstructions : public CpuTest
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

    std::vector< u8 > zero_page_data;
    zero_page_data.reserve(0x100);
    for (u16 i = 1; i < 0x100; ++i)
      zero_page_data.push_back(i & 0xFF);

    memory_maps.insert({
      0x1,
      MemoryLayer(
        "Zero Page",
        std::move(zero_page_data)
      )
    });
  }

  inline static constexpr Addr TestProgramStart{program_start + 6};
  void LoadRegisters()
  {
    constexpr u8 lda_addr{0xC2};
    constexpr u8 ldx_addr{0x21};
    constexpr u8 ldy_addr{0x37};

    memory_maps.insert({
      program_start,
      MemoryLayer(
        "Program Memory",
        std::vector< Instruction >{
          Instruction::zero_page(ZeroPageOpcodes::LDA, lda_addr),
          Instruction::zero_page(ZeroPageOpcodes::LDX, ldx_addr),
          Instruction::zero_page(ZeroPageOpcodes::LDY, ldy_addr),
        }
      )
    });

    reset_to_entry();
    tick(9);

    {
      SCOPED_TRACE("LoadRegisters");
      ASSERT_EQ(cpu->REG_A, lda_addr);
      ASSERT_EQ(cpu->REG_X, ldx_addr);
      ASSERT_EQ(cpu->REG_Y, ldy_addr);
      ASSERT_EQ(cpu->PC, program_start + 7);
    }
  }
};

TEST_F(ZeroPageInstructions, LoadRegisterTests)
{
  constexpr u8 lda_addr{0x42};
  constexpr u8 ldx_addr{0x21};
  constexpr u8 ldy_addr{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::LDA, lda_addr),
        Instruction::zero_page(ZeroPageOpcodes::LDX, ldx_addr),
        Instruction::zero_page(ZeroPageOpcodes::LDY, ldy_addr),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 2); // LDX instruction

  {
    SCOPED_TRACE("LDA load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 1);
  }

  tick();

  {
    SCOPED_TRACE("LDA load zero page");
    expect_regs_change({});
    expect_bus_read(lda_addr);
  }

  tick();

  {
    SCOPED_TRACE("LDA load value to register");
    expect_regs_change({.pc = NEXT_PC, .a = lda_addr});
    expect_bus_read(program_start + 2);
  }

  tick(); // Decoding
  tick(2);

  {
    SCOPED_TRACE("LDX load value to register");
    expect_regs_change({.pc = NEXT_PC, .x = ldx_addr});
    expect_bus_read(program_start + 4);
  }

  tick(); // Decoding
  tick(2);

  {
    SCOPED_TRACE("LDY load value to register");
    expect_regs_change({.pc = NEXT_PC, .y = ldy_addr});
    expect_bus_read(program_start + 6);
  }
}

TEST_F(ZeroPageInstructions, StoreRegisterTests)
{
  constexpr u8 sta_addr{0x72};
  constexpr u8 stx_addr{0x71};
  constexpr u8 sty_addr{0x77};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::STX, stx_addr),
        Instruction::zero_page(ZeroPageOpcodes::STY, sty_addr),
        Instruction::zero_page(ZeroPageOpcodes::STA, sta_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("STX load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("STX store register");
    expect_regs_change({});
    expect_bus_write(stx_addr, cpu->REG_X);
  }

  tick();
  {
    SCOPED_TRACE("STX read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick(); // Decoding
  tick(3);
  {
    SCOPED_TRACE("STY");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 4);
    EXPECT_EQ(get_memory(sty_addr, false), cpu->REG_Y);
  }

  tick(); // Decoding
  tick(3);
  {
    SCOPED_TRACE("STA");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 6);
    EXPECT_EQ(get_memory(sta_addr, false), cpu->REG_A);
  }
}

TEST_F(ZeroPageInstructions, BitTest)
{
  constexpr u8 bit1_addr{0x80};
  constexpr u8 bit2_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::BIT, bit1_addr),
        Instruction::zero_page(ZeroPageOpcodes::BIT, bit2_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("BIT load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(2);
  {
    SCOPED_TRACE("BIT1 read value");
    expect_flags_change({
      .zero = !(bit1_addr & cpu->REG_A),
      .overflow = !!(bit1_addr & 0x40),
      .negative = !!(bit1_addr & 0x80)
    });
  }

  tick(); // Decode
  tick(3);
  {
    SCOPED_TRACE("BIT2 read value");
    expect_flags_change({
      .zero = !(bit2_addr & cpu->REG_A),
      .overflow = !!(bit2_addr & 0x40),
      .negative = !!(bit2_addr & 0x80)
    });
  }
}

TEST_F(ZeroPageInstructions, BitOpsTest)
{
  constexpr u8 or_addr{0x80};
  constexpr u8 and_addr{0x3f};
  constexpr u8 xor_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::ORA, or_addr),
        Instruction::zero_page(ZeroPageOpcodes::AND, and_addr),
        Instruction::zero_page(ZeroPageOpcodes::EOR, xor_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("ORA load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(2);
  {
    SCOPED_TRACE("ORA perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | or_addr});
  }

  tick(); // Decode
  tick(3);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & and_addr});
  }

  tick(); // Decode
  tick(3);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ xor_addr});
  }
}

TEST_F(ZeroPageInstructions, AddSbcTest)
{
  constexpr u8 adc_addr{0x80};
  constexpr u8 sbc_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::ADC, adc_addr),
        Instruction::zero_page(ZeroPageOpcodes::SBC, sbc_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("ADC load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(2);
  {
    SCOPED_TRACE("ADC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + adc_addr});
  }

  tick(); // Decode
  tick(3);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - sbc_addr - !*prev_flags.carry});
  }
}

TEST_F(ZeroPageInstructions, IncDecTest)
{
  constexpr u8 inc_addr{0x80};
  constexpr u8 dec_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::INC, inc_addr),
        Instruction::zero_page(ZeroPageOpcodes::DEC, dec_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("ADC load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("ADC read data");
    expect_bus_read(inc_addr);
  }

  tick();
  {
    SCOPED_TRACE("ADC write data");
    expect_bus_write(inc_addr, inc_addr + 1);
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("SBC perform op");
    expect_bus_write(dec_addr, dec_addr - 1);
  }
}

TEST_F(ZeroPageInstructions, ShiftOpTest)
{
  constexpr u8 asl_addr{0x6b};
  constexpr u8 lsr_addr{0x3f};
  constexpr u8 rol_addr{0x80};
  constexpr u8 ror_addr{0x7f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::ASL, asl_addr),
        Instruction::zero_page(ZeroPageOpcodes::LSR, lsr_addr),
        Instruction::zero_page(ZeroPageOpcodes::ROL, rol_addr),
        Instruction::zero_page(ZeroPageOpcodes::ROR, ror_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("ASL load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("ASL read data");
    expect_bus_read(asl_addr);
  }

  tick();
  {
    SCOPED_TRACE("ASL write data");
    expect_bus_write(asl_addr, asl_addr << 1);
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("LSR perform op");
    expect_bus_write(lsr_addr, lsr_addr >> 1);
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("ROL perform op");
    expect_bus_write(rol_addr, rol(rol_addr));
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("ROR perform op");
    expect_bus_write(ror_addr, ror(ror_addr));
  }
}

TEST_F(ZeroPageInstructions, CmpTest)
{
  constexpr u8 cmp_addr{0x80};
  constexpr u8 cpx_addr{0x3f};
  constexpr u8 cpy_addr{0x15};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageOpcodes::CMP, cmp_addr),
        Instruction::zero_page(ZeroPageOpcodes::CPX, cpx_addr),
        Instruction::zero_page(ZeroPageOpcodes::CPY, cpy_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("CMP load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(2);
  {
    SCOPED_TRACE("CMP perform compare");
    expect_flags_change({
      .carry = true
    });
  }

  tick(); // Decode
  tick(3);
  {
    SCOPED_TRACE("CPX read value");
    expect_flags_change({
      .carry = false,
      .negative = true
    });
  }

  tick(); // Decode
  tick(3);
  {
    SCOPED_TRACE("CPY read value");
    expect_flags_change({
      .carry = true,
      .negative = false
    });
  }
}
