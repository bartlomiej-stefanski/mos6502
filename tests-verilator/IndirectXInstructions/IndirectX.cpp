#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "MemoryArea.hpp"

constexpr Addr program_start{0x8090};

class IndirectXInstructions : public CpuTest
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
    constexpr u8 ldx_val{0x22};
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

TEST_F(IndirectXInstructions, LoadRegisterTests)
{
  constexpr u8 lda_val{0x42};

  memory_maps.insert({
    program_start,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectXOpcodes::LDA, PointerTable + lda_val * 2),
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
  const Addr load_a_addr = lda_val * 2 + *prev_state.x;
  {
    SCOPED_TRACE("LDA load addr low");
    expect_regs_change({});
    expect_bus_read(PointerTable + load_a_addr);
  }

  tick();
  {
    SCOPED_TRACE("LDA load addr high");
    expect_regs_change({});
    expect_bus_read(PointerTable + load_a_addr + 1);
  }

  tick();
  {
    SCOPED_TRACE("LDA load value");
    expect_regs_change({});
    expect_bus_read(MemoryPage + lda_val);
  }

  tick();
  {
    SCOPED_TRACE("LDA store value in A");
    expect_regs_change({.pc = NEXT_PC, .a = lda_val});
  }
}

TEST_F(IndirectXInstructions, StoreRegisterTests)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectXOpcodes::STA, PointerTable),
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
    expect_bus_read(PointerTable + *prev_state.x);
  }

  tick();
  {
    SCOPED_TRACE("STA load addr high");
    expect_regs_change({});
    expect_bus_read(PointerTable + *prev_state.x + 1);
  }

  tick();
  {
    SCOPED_TRACE("STA store register");
    expect_regs_change({});
    expect_bus_write(MemoryPage + *prev_state.x / 2, cpu->REG_A);
  }

  tick();
  {
    SCOPED_TRACE("STA read next opcode");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 3);
  }
}

TEST_F(IndirectXInstructions, BitOpsTest)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectXOpcodes::ORA, PointerTable),
        Instruction::indirect(IndirectXOpcodes::AND, PointerTable),
        Instruction::indirect(IndirectXOpcodes::EOR, PointerTable),
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
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | (*prev_state.x / 2)});
  }

  tick(6);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & (*prev_state.x / 2)});
  }

  tick(6);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ (*prev_state.x / 2)});
  }
}

TEST_F(IndirectXInstructions, AddSbcTest)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectXOpcodes::ADC, PointerTable),
        Instruction::indirect(IndirectXOpcodes::SBC, PointerTable),
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
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + (*prev_state.x / 2)});
  }

  tick(6);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - (*prev_state.x / 2) - !*prev_flags.carry});
  }
}

TEST_F(IndirectXInstructions, CmpTest)
{
  memory_maps.insert({
    TestProgramStart,
    std::unique_ptr< MemoryArea >(new MemoryObject(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::indirect(IndirectXOpcodes::CMP, PointerTable),
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
