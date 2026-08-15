#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "CpuTest.hpp"
#include "MemoryArea.hpp"

constexpr Addr program_start{0x8090};

class IndirectYInstructions : public CpuTest
{
protected:
  inline static constexpr Addr MemoryPage{0xa000};
  inline static constexpr Addr PointerTable{0xd000};
  inline static constexpr Addr RamAddr{0x2000};
  void SetUpMemory() override
  {
    CpuTest::SetUpMemory();

    memory_maps.insert({
      ResetVector,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "ResetVector",
        {MO(program_start)}
      ))
    });

    std::vector< u8 > romData;
    romData.reserve(0x100);
    for (u16 i = 0; i < 0x100; ++i)
      romData.push_back(i & 0xFF);

    memory_maps.insert({
      MemoryPage,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "Zero Page",
        std::move(romData)
      ))
    });

    memory_maps.insert({
      RamAddr,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "RAM",
        std::vector< u8 >(512)
      ))
    });

    std::vector< MemoryOccupant > pointers;
    for (u16 i = 0; i < 0x50; i++)
      pointers.push_back(MO((Addr)(MemoryPage + i)));

    memory_maps.insert({
      PointerTable,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "Pointer Table",
        std::move(pointers)
      ))
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
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "Program Memory",
        std::vector< Instruction >{
          Instruction::absolute(AbsoluteOpcodes::LDA, MemoryPage + lda_val),
          Instruction::absolute(AbsoluteOpcodes::LDX, MemoryPage + ldx_val),
          Instruction::absolute(AbsoluteOpcodes::LDY, MemoryPage + ldy_val),
        }
      ))
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

TEST_F(IndirectYInstructions, LoadRegisterTests)
{
  memory_maps.insert({
    program_start,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectYOpcodes::LDA, PointerTable),
        Instruction::nop()
      }
    ))
  });

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 3); // LDX instruction, load indirect low

  {
    SCOPED_TRACE("LDA load indirect-addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(program_start + 2);
  }

  tick();
  {
    SCOPED_TRACE("LDA load addr low");
    expect_regs_change({});
    expect_bus_read(PointerTable);
  }

  tick();
  {
    SCOPED_TRACE("LDA load addr high");
    expect_regs_change({});
    expect_bus_read(PointerTable + 1);
  }

  tick();
  {
    SCOPED_TRACE("LDA load value");
    expect_regs_change({});
    expect_bus_read(MemoryPage + *prev_state.y);
  }

  tick();
  {
    SCOPED_TRACE("LDA store value in A");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.y});
  }
}

TEST_F(IndirectYInstructions, StoreRegisterTests)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectYOpcodes::STA, PointerTable),
        Instruction::nop()
      }
    ))
  });

  LoadRegisters();

  {
    SCOPED_TRACE("STA load indirect-addr low");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick();
  {
    SCOPED_TRACE("STA load indirect-addr high");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 2);
  }

  tick();
  {
    SCOPED_TRACE("STA load addr low");
    expect_regs_change({});
    expect_bus_read(PointerTable);
  }

  tick();
  {
    SCOPED_TRACE("STA load addr high");
    expect_regs_change({});
    expect_bus_read(PointerTable + 1);
  }

  tick();
  {
    SCOPED_TRACE("STA store register");
    expect_regs_change({});
    expect_bus_write(MemoryPage + *prev_state.y, cpu->REG_A);
  }

  tick();
  {
    SCOPED_TRACE("STA read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 3);
  }
}

TEST_F(IndirectYInstructions, BitOpsTest)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectYOpcodes::ORA, PointerTable),
        Instruction::indirect(IndirectYOpcodes::AND, PointerTable),
        Instruction::indirect(IndirectYOpcodes::EOR, PointerTable),
        Instruction::nop()
      }
    ))
  });

  LoadRegisters();

  {
    SCOPED_TRACE("ORA load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(5);
  {
    SCOPED_TRACE("ORA perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | *prev_state.y});
  }

  tick(6);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & *prev_state.y});
  }

  tick(6);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ *prev_state.y});
  }
}

TEST_F(IndirectYInstructions, AddSbcTest)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectYOpcodes::ADC, PointerTable),
        Instruction::indirect(IndirectYOpcodes::SBC, PointerTable),
        Instruction::nop()
      }
    ))
  });

  LoadRegisters();

  {
    SCOPED_TRACE("ADC load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(5);
  {
    SCOPED_TRACE("ADC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + *prev_state.y});
  }

  tick(6);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - *prev_state.y - !*prev_flags.carry});
  }
}

TEST_F(IndirectYInstructions, CmpTest)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectYOpcodes::CMP, PointerTable),
        Instruction::nop()
      }
    ))
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
    expect_flags_change({});
  }
}
