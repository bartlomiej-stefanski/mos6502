#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "BitHelper.hpp"

constexpr Addr program_start{0x8090};

class AbsoluteYInstructions : public CpuTest
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

TEST_F(AbsoluteYInstructions, LoadRegisterTests)
{
  constexpr u8 lda_val{0x42};
  constexpr u8 ldx_val{0x37};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteYOpcodes::LDA, MemoryPage + lda_val),
        Instruction::absolute(AbsoluteYOpcodes::LDX, MemoryPage + ldx_val),
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

  const Addr load_a_addr = lda_val + *prev_state.y;
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

  const Addr load_y_addr = ldx_val + *prev_state.y;
  {
    SCOPED_TRACE("LDX load value to register");
    expect_regs_change({.pc = NEXT_PC, .x = load_y_addr & 0xFF});
    expect_bus_read(program_start + 6);
  }
}

TEST_F(AbsoluteYInstructions, StoreRegisterTests)
{
  constexpr Addr sta_addr{RamAddr + 0x72};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteYOpcodes::STA, sta_addr),
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
    expect_bus_write(sta_addr + *prev_state.y, cpu->REG_A);
  }

  tick();
  {
    SCOPED_TRACE("STA read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 3);
  }
}

TEST_F(AbsoluteYInstructions, BitOpsTest)
{
  constexpr Addr or_addr{MemoryPage + 0x80};
  constexpr Addr and_addr{MemoryPage + 0x3f};
  constexpr Addr xor_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteYOpcodes::ORA, or_addr),
        Instruction::absolute(AbsoluteYOpcodes::AND, and_addr),
        Instruction::absolute(AbsoluteYOpcodes::EOR, xor_addr),
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
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | (or_addr + *prev_state.y)});
  }

  tick(); // Decode
  tick(4);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & (and_addr + *prev_state.y)});
  }

  tick(); // Decode
  tick(4);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ (xor_addr + *prev_state.y)});
  }
}

TEST_F(AbsoluteYInstructions, AddSbcTest)
{
  constexpr Addr adc_addr{MemoryPage + 0x80};
  constexpr Addr sbc_addr{MemoryPage + 0x3f};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteYOpcodes::ADC, adc_addr),
        Instruction::absolute(AbsoluteYOpcodes::SBC, sbc_addr),
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
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + adc_addr + *prev_state.y});
  }

  tick(); // Decode
  tick(4);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - (sbc_addr + *prev_state.y) - !*prev_flags.carry});
  }
}

TEST_F(AbsoluteYInstructions, CmpTest)
{
  constexpr Addr cmp_addr{MemoryPage + 0x80};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::absolute(AbsoluteYOpcodes::CMP, cmp_addr),
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
      .negative = true
    });
  }
}
