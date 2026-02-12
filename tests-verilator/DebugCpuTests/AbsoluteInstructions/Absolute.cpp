#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "BitHelper.hpp"

constexpr Addr program_start{0x8090};

class AbsoluteInstructions : public CpuTest
{
protected:
  inline static constexpr Addr MemoryPage{0xa000};
  inline static constexpr Addr RamAddr{0x2000};
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

    std::vector< u8 > romData;
    romData.reserve(0x100);
    for (u16 i = 0; i < 0x100; ++i)
      romData.push_back(i & 0xFF);

    memory_maps.insert({
      MemoryPage,
      MemoryLayer(
        "Zero Page",
        std::move(romData)
      )
    });

    memory_maps.insert({
      RamAddr,
      MemoryLayer(
        "RAM",
        std::vector< u8 >(512)
      )
    });
  }

  inline static constexpr Addr TestProgramStart{program_start + 9};
  void LoadRegisters()
  {
    constexpr u8 lda_val{0xC2};
    constexpr u8 ldx_val{0x21};
    constexpr u8 ldy_val{0x37};

    memory_maps.insert({
      program_start,
      MemoryLayer(
        "Program Memory",
        std::vector< Instruction >{
          Instruction::absolute(AbsoluteOpcodes::LDA, MemoryPage + lda_val),
          Instruction::absolute(AbsoluteOpcodes::LDX, MemoryPage + ldx_val),
          Instruction::absolute(AbsoluteOpcodes::LDY, MemoryPage + ldy_val),
        }
      )
    });

    reset_to_entry();
    tick(11);

    {
      SCOPED_TRACE("LoadRegisters");
      ASSERT_EQ(cpu->REG_A, lda_val);
      ASSERT_EQ(cpu->REG_X, ldx_val);
      ASSERT_EQ(cpu->REG_Y, ldy_val);
      ASSERT_EQ(cpu->PC, TestProgramStart + 2);
    }
  }
};

TEST_F(AbsoluteInstructions, LoadRegisterTests)
{
  constexpr u8 lda_val{0x42};
  constexpr u8 ldx_val{0x21};
  constexpr u8 ldy_val{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::LDA, MemoryPage + lda_val),
        Instruction::absolute(AbsoluteOpcodes::LDX, MemoryPage + ldx_val),
        Instruction::absolute(AbsoluteOpcodes::LDY, MemoryPage + ldy_val),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3); // LDX instruction, load low addr

  {
    SCOPED_TRACE("LDA load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 2);
  }

  tick();

  {
    SCOPED_TRACE("LDA load from addr");
    expect_regs_change({});
    expect_bus_read(MemoryPage + lda_val);
  }

  tick();

  {
    SCOPED_TRACE("LDA load value to register");
    expect_regs_change({.pc = NEXT_PC, .a = lda_val});
    expect_bus_read(program_start + 3);
  }

  tick(4);

  {
    SCOPED_TRACE("LDX load value to register");
    expect_regs_change({.pc = NEXT_PC, .x = ldx_val});
    expect_bus_read(program_start + 6);
  }

  tick(4);

  {
    SCOPED_TRACE("LDY load value to register");
    expect_regs_change({.pc = NEXT_PC, .y = ldy_val});
    expect_bus_read(program_start + 9);
  }
}

TEST_F(AbsoluteInstructions, StoreRegisterTests)
{
  constexpr Addr sta_addr{RamAddr + 0x72};
  constexpr Addr stx_addr{RamAddr + 0x71};
  constexpr Addr sty_addr{RamAddr + 0x77};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::STX, stx_addr),
        Instruction::absolute(AbsoluteOpcodes::STY, sty_addr),
        Instruction::absolute(AbsoluteOpcodes::STA, sta_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("STX load addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("STX load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
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
    expect_bus_read(TestProgramStart + 3);
  }

  tick(4);
  {
    SCOPED_TRACE("STY");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 6);
    EXPECT_EQ(get_memory(sty_addr, false), cpu->REG_Y);
  }

  tick(4);
  {
    SCOPED_TRACE("STA");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 9);
    EXPECT_EQ(get_memory(sta_addr, false), cpu->REG_A);
  }
}

TEST_F(AbsoluteInstructions, BitTest)
{
  constexpr Addr bit1_addr{MemoryPage + 0x80};
  constexpr Addr bit2_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::BIT, bit1_addr),
        Instruction::absolute(AbsoluteOpcodes::BIT, bit2_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("BIT load low addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(3);
  {
    SCOPED_TRACE("BIT1 read value");
    expect_flags_change({
      .zero = !(bit1_addr & cpu->REG_A),
      .overflow = !!(bit1_addr & 0x40),
      .negative = !!(bit1_addr & 0x80)
    });
  }

  tick(4);
  {
    SCOPED_TRACE("BIT2 read value");
    expect_flags_change({
      .zero = !(bit2_addr & cpu->REG_A),
      .overflow = !!(bit2_addr & 0x40),
      .negative = !!(bit2_addr & 0x80)
    });
  }
}

TEST_F(AbsoluteInstructions, BitOpsTest)
{
  constexpr Addr or_addr{MemoryPage + 0x80};
  constexpr Addr and_addr{MemoryPage + 0x3f};
  constexpr Addr xor_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::ORA, or_addr),
        Instruction::absolute(AbsoluteOpcodes::AND, and_addr),
        Instruction::absolute(AbsoluteOpcodes::EOR, xor_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("ORA load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(3);
  {
    SCOPED_TRACE("ORA perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | or_addr});
  }

  tick(4);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & and_addr});
  }

  tick(4);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ xor_addr});
  }
}

TEST_F(AbsoluteInstructions, AddSbcTest)
{
  constexpr Addr adc_addr{MemoryPage + 0x80};
  constexpr Addr sbc_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::ADC, adc_addr),
        Instruction::absolute(AbsoluteOpcodes::SBC, sbc_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("ADC load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(3);
  {
    SCOPED_TRACE("ADC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + adc_addr});
  }

  tick(4);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - sbc_addr - !*prev_flags.carry});
  }
}

TEST_F(AbsoluteInstructions, IncDecTest)
{
  constexpr Addr inc_addr{MemoryPage + 0x80};
  constexpr Addr dec_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::INC, inc_addr),
        Instruction::absolute(AbsoluteOpcodes::DEC, dec_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("INC load addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("INC load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick();
  {
    SCOPED_TRACE("INC read data");
    expect_bus_read(inc_addr);
  }

  tick();
  {
    SCOPED_TRACE("INC write data");
    expect_bus_write(inc_addr, (inc_addr & 0xFF) + 1);
  }

  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("DEC perform op");
    expect_bus_write(dec_addr, (dec_addr & 0xFF) - 1);
  }
}

TEST_F(AbsoluteInstructions, ShiftOpTest)
{
  constexpr Addr asl_addr{MemoryPage + 0x6b};
  constexpr Addr lsr_addr{MemoryPage + 0x3f};
  constexpr Addr rol_addr{MemoryPage + 0x80};
  constexpr Addr ror_addr{MemoryPage + 0x7f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::ASL, asl_addr),
        Instruction::absolute(AbsoluteOpcodes::LSR, lsr_addr),
        Instruction::absolute(AbsoluteOpcodes::ROL, rol_addr),
        Instruction::absolute(AbsoluteOpcodes::ROR, ror_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("ASL load addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("ASL load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick();
  {
    SCOPED_TRACE("ASL read data");
    expect_bus_read(asl_addr);
  }

  tick();
  {
    SCOPED_TRACE("ASL write data");
    expect_bus_write(asl_addr, (asl_addr << 1) & 0xFF);
  }

  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("LSR perform op");
    expect_bus_write(lsr_addr, (lsr_addr >> 1) & 0xFF);
  }

  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("ROL perform op");
    expect_bus_write(rol_addr, rol(rol_addr & 0xFF));
  }

  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("ROR perform op");
    expect_bus_write(ror_addr, ror(ror_addr & 0xFF));
  }
}

TEST_F(AbsoluteInstructions, CmpTest)
{
  constexpr Addr cmp_addr{MemoryPage + 0x80};
  constexpr Addr cpx_addr{MemoryPage + 0x3f};
  constexpr Addr cpy_addr{MemoryPage + 0x15};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteOpcodes::CMP, cmp_addr),
        Instruction::absolute(AbsoluteOpcodes::CPX, cpx_addr),
        Instruction::absolute(AbsoluteOpcodes::CPY, cpy_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  {
    SCOPED_TRACE("CMP load addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("CMP load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick(2);
  {
    SCOPED_TRACE("CMP perform compare");
    expect_flags_change({
      .carry = true
    });
  }

  tick(4);
  {
    SCOPED_TRACE("CPX read value");
    expect_flags_change({
      .carry = false,
      .negative = true
    });
  }

  tick(4);
  {
    SCOPED_TRACE("CPY read value");
    expect_flags_change({
      .carry = true,
      .negative = false
    });
  }
}
