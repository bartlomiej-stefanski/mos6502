#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "BitHelper.hpp"

constexpr Addr program_start{0x8090};

class ZeroPageXInstructions : public CpuTest
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
      zero_page_data.push_back((256 - i) & 0xFF);

    memory_maps.insert({
      0x1,
      MemoryLayer(
        "Zero Page",
        std::move(zero_page_data)
      )
    });

    std::vector< u8 > one_page_data;
    zero_page_data.reserve(0x100);
    for (u16 i = 0; i < 0x100; ++i)
      one_page_data.push_back((256 - i) & 0xFF);

    memory_maps.insert({
      0x100,
      MemoryLayer(
        "Zero Page",
        std::move(one_page_data)
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
          Instruction::zero_page(ZeroPageOpcodes::LDA, 256 - lda_addr),
          Instruction::zero_page(ZeroPageOpcodes::LDX, 256 - ldx_addr),
          Instruction::zero_page(ZeroPageOpcodes::LDY, 256 - ldy_addr),
        }
      )
    });

    reset_to_entry();
    tick(7);

    {
      SCOPED_TRACE("LoadRegisters");
      ASSERT_EQ(cpu->REG_A, lda_addr);
      ASSERT_EQ(cpu->REG_X, ldx_addr);
      ASSERT_EQ(cpu->REG_Y, ldy_addr);
      ASSERT_EQ(cpu->PC, TestProgramStart + 1);
    }
  }
};

TEST_F(ZeroPageXInstructions, LoadRegisterTests)
{
  constexpr u8 lda_addr{0x42};
  constexpr u8 ldy_addr{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageXOpcodes::LDA, 256 - lda_addr),
        Instruction::zero_page(ZeroPageXOpcodes::LDY, 256 - ldy_addr),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 2); // LDX instruction, loading immediate

  {
    SCOPED_TRACE("LDA load zero page");
    expect_regs_change({});
    expect_bus_read(256 - (lda_addr + *prev_state.x));
  }

  tick();

  {
    SCOPED_TRACE("LDA load value to register");
    expect_regs_change({.pc = NEXT_PC, .a = lda_addr + *prev_state.x});
    expect_bus_read(program_start + 2);
  }

  tick(3);

  {
    SCOPED_TRACE("LDY load value to register");
    expect_regs_change({.pc = NEXT_PC, .y = ldy_addr + *prev_state.x});
    expect_bus_read(program_start + 4);
  }
}

TEST_F(ZeroPageXInstructions, StoreRegisterTests)
{
  constexpr u8 sta_addr{0x72};
  constexpr u8 sty_addr{0x77};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageXOpcodes::STY, sty_addr),
        Instruction::zero_page(ZeroPageXOpcodes::STA, sta_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("STY load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("STY store register");
    expect_regs_change({});
    expect_bus_write(sty_addr + *prev_state.x, cpu->REG_Y);
  }

  tick();
  {
    SCOPED_TRACE("STY read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick(3);
  {
    SCOPED_TRACE("STA");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 4);
    EXPECT_EQ(get_memory(sta_addr + *prev_state.x, false), cpu->REG_A);
  }
}

TEST_F(ZeroPageXInstructions, BitOpsTest)
{
  constexpr u8 or_addr{0x80};
  constexpr u8 and_addr{0x3f};
  constexpr u8 xor_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageXOpcodes::ORA, 256 - or_addr),
        Instruction::zero_page(ZeroPageXOpcodes::AND, 256 - and_addr),
        Instruction::zero_page(ZeroPageXOpcodes::EOR, 256 - xor_addr),
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
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | (or_addr - *prev_state.x)});
  }

  tick(3);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & (and_addr - *prev_state.x)});
  }

  tick(3);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ (xor_addr - *prev_state.x)});
  }
}

TEST_F(ZeroPageXInstructions, AddSbcTest)
{
  constexpr u8 adc_addr{0x80};
  constexpr u8 sbc_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageXOpcodes::ADC, 256 - adc_addr),
        Instruction::zero_page(ZeroPageXOpcodes::SBC, 256 - sbc_addr),
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
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + (adc_addr - *prev_state.x)});
  }

  tick(3);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - (sbc_addr - *prev_state.x) - !*prev_flags.carry});
  }
}

TEST_F(ZeroPageXInstructions, IncDecTest)
{
  constexpr u8 inc_addr{0x80};
  constexpr u8 dec_addr{0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageXOpcodes::INC, 256 - inc_addr),
        Instruction::zero_page(ZeroPageXOpcodes::DEC, 256 - dec_addr),
        Instruction::nop()
      }
    )
  });

  LoadRegisters();
  tick();

  {
    SCOPED_TRACE("INC load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  const Addr inc_val = inc_addr - *prev_state.x;
  tick();
  {
    SCOPED_TRACE("INC read data");
    expect_bus_read(inc_addr + *prev_state.x); }

  tick();
  {
    SCOPED_TRACE("INC write data");
    expect_bus_write(256 - inc_val, inc_val + 1);
  }

  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("DEC perform op");
    expect_bus_write(256 - (dec_addr - *prev_state.x), (dec_addr - *prev_state.x) - 1);
  }
}

TEST_F(ZeroPageXInstructions, ShiftOpTest)
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
        Instruction::zero_page(ZeroPageXOpcodes::ASL, 256 - asl_addr),
        Instruction::zero_page(ZeroPageXOpcodes::LSR, 256 - lsr_addr),
        Instruction::zero_page(ZeroPageXOpcodes::ROL, 256 - rol_addr),
        Instruction::zero_page(ZeroPageXOpcodes::ROR, 256 - ror_addr),
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
  const Addr asl_val = asl_addr - *prev_state.x;
  {
    SCOPED_TRACE("ASL read data");
    expect_bus_read(256 - asl_val);
  }

  tick();
  {
    SCOPED_TRACE("ASL write data");
    expect_bus_write(256 - asl_val, asl_val << 1);
  }

  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("LSR perform op");
    expect_bus_write(256 - (lsr_addr - *prev_state.x), (lsr_addr - *prev_state.x) >> 1);
  }

  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("ROL perform op");
    expect_bus_write(256 - (rol_addr - *prev_state.x), rol(rol_addr - *prev_state.x));
  }

  tick(); // Decode

  tick(3);
  {
    SCOPED_TRACE("ROR perform op");
    expect_bus_write(256 - (ror_addr - *prev_state.x), ror(ror_addr - *prev_state.x));
  }
}

TEST_F(ZeroPageXInstructions, CmpTest)
{
  constexpr u8 cmp_addr{0x80};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageXOpcodes::CMP, 256 - cmp_addr),
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
      .carry = true,
      .overflow = true
    });
  }
}
