#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "BitHelper.hpp"

constexpr Addr program_start{0x8090};

class AbsoluteXInstructions : public CpuTest
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
    tick(14);

    {
      SCOPED_TRACE("LoadRegisters");
      ASSERT_EQ(cpu->REG_A, lda_val);
      ASSERT_EQ(cpu->REG_X, ldx_val);
      ASSERT_EQ(cpu->REG_Y, ldy_val);
      ASSERT_EQ(cpu->PC, TestProgramStart + 1);
    }
  }
};

TEST_F(AbsoluteXInstructions, LoadRegisterTests)
{
  constexpr u8 lda_val{0x42};
  constexpr u8 ldy_val{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteXOpcodes::LDA, MemoryPage + lda_val),
        Instruction::absolute(AbsoluteXOpcodes::LDY, MemoryPage + ldy_val),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 2); // LDX instruction

  {
    SCOPED_TRACE("LDA load addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 1);
  }

  tick();

  {
    SCOPED_TRACE("LDA load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 2);
  }

  tick();

  const Addr load_a_addr = lda_val + *prev_state.x;
  {
    SCOPED_TRACE("LDA load from addr");
    expect_regs_change({});
    expect_bus_read(MemoryPage + load_a_addr);
  }

  tick();

  {
    SCOPED_TRACE("LDA load value to register");
    expect_regs_change({.pc = NEXT_PC, .a = load_a_addr & 0xFF});
    expect_bus_read(program_start + 3);
  }

  tick(); // Decoding
  tick(4);

  const Addr load_y_addr = ldy_val + *prev_state.x;
  {
    SCOPED_TRACE("LDY load value to register");
    expect_regs_change({.pc = NEXT_PC, .y = load_y_addr & 0xFF});
    expect_bus_read(program_start + 6);
  }
}

TEST_F(AbsoluteXInstructions, StoreRegisterTests)
{
  constexpr Addr sta_addr{RamAddr + 0x72};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteXOpcodes::STA, sta_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("STA load addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("STA load addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick();
  {
    SCOPED_TRACE("STA store register");
    expect_regs_change({});
    expect_bus_write(sta_addr + *prev_state.x, cpu->REG_A);
  }

  tick();
  {
    SCOPED_TRACE("STA read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 3);
  }
}

TEST_F(AbsoluteXInstructions, BitOpsTest)
{
  constexpr Addr or_addr{MemoryPage + 0x80};
  constexpr Addr and_addr{MemoryPage + 0x3f};
  constexpr Addr xor_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteXOpcodes::ORA, or_addr),
        Instruction::absolute(AbsoluteXOpcodes::AND, and_addr),
        Instruction::absolute(AbsoluteXOpcodes::EOR, xor_addr),
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

  tick(3);
  {
    SCOPED_TRACE("ORA perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | (or_addr + *prev_state.x)});
  }

  tick(); // Decode
  tick(4);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & (and_addr + *prev_state.x)});
  }

  tick(); // Decode
  tick(4);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ (xor_addr + *prev_state.x)});
  }
}

TEST_F(AbsoluteXInstructions, AddSbcTest)
{
  constexpr Addr adc_addr{MemoryPage + 0x80};
  constexpr Addr sbc_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteXOpcodes::ADC, adc_addr),
        Instruction::absolute(AbsoluteXOpcodes::SBC, sbc_addr),
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

  tick(3);
  {
    SCOPED_TRACE("ADC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + adc_addr + *prev_state.x});
  }

  tick(); // Decode
  tick(4);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - (sbc_addr + *prev_state.x) - !*prev_flags.carry});
  }
}

TEST_F(AbsoluteXInstructions, CmpTest)
{
  constexpr Addr cmp_addr{MemoryPage + 0x80};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteXOpcodes::CMP, cmp_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  tick();
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
}

TEST_F(AbsoluteXInstructions, ShiftOpTest)
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
        Instruction::absolute(AbsoluteXOpcodes::ASL, asl_addr),
        Instruction::absolute(AbsoluteXOpcodes::LSR, lsr_addr),
        Instruction::absolute(AbsoluteXOpcodes::ROL, rol_addr),
        Instruction::absolute(AbsoluteXOpcodes::ROR, ror_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  tick();
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

  const Addr asl_val = asl_addr + *prev_state.x;
  tick();
  {
    SCOPED_TRACE("ASL read data");
    expect_bus_read(asl_val);
  }

  tick();
  {
    SCOPED_TRACE("ASL write data");
    expect_bus_write(asl_val, (asl_val << 1) & 0xFF);
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("LSR perform op");
    expect_bus_write(lsr_addr + *prev_state.x, ((lsr_addr + *prev_state.x) >> 1) & 0xFF);
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("ROL perform op");
    expect_bus_write(rol_addr + *prev_state.x, rol((rol_addr + *prev_state.x) & 0xFF));
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("ROR perform op");
    expect_bus_write(ror_addr + *prev_state.x, ror((ror_addr + *prev_state.x) & 0xFF));
  }
}

TEST_F(AbsoluteXInstructions, IncDecTest)
{
  constexpr Addr inc_addr{MemoryPage + 0x80};
  constexpr Addr dec_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteXOpcodes::INC, inc_addr),
        Instruction::absolute(AbsoluteXOpcodes::DEC, dec_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();

  tick();
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
    expect_bus_read(inc_addr + *prev_state.x);
  }

  tick();
  {
    SCOPED_TRACE("INC write data");
    expect_bus_write(inc_addr + *prev_state.x, (inc_addr + *prev_state.x + 1) & 0xFF);
  }

  tick(); // Get next opcode
  tick(); // Decode

  tick(4);
  {
    SCOPED_TRACE("DEC perform op");
    expect_bus_write(dec_addr + *prev_state.x, (dec_addr + *prev_state.x - 1) & 0xFF);
  }
}
