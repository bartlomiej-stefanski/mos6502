#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"

constexpr Addr program_start{0x8090};

class ZeroPageYInstructions : public CpuTest
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

TEST_F(ZeroPageYInstructions, LoadRegisterTests)
{
  constexpr u8 ldx_addr{0x42};

  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageYOpcodes::LDX, 256 - ldx_addr),
        Instruction::nop()
      }
    )
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 2); // LDX instruction, loading immediate

  {
    SCOPED_TRACE("LDX load zero page");
    expect_regs_change({});
    expect_bus_read(256 - (ldx_addr + *prev_state.y));
  }

  tick();

  {
    SCOPED_TRACE("LDX load value to register");
    expect_regs_change({.pc = NEXT_PC, .x = ldx_addr + *prev_state.y});
    expect_bus_read(program_start + 2);
  }
}

TEST_F(ZeroPageYInstructions, StoreRegisterTests)
{
  constexpr u8 stx_addr{0x72};

  memory_maps.insert({
    TestProgramStart,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::zero_page(ZeroPageYOpcodes::STX, stx_addr),
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
    expect_bus_write(stx_addr + *prev_state.y, cpu->REG_X);
  }

  tick();
  {
    SCOPED_TRACE("STX read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }
}
