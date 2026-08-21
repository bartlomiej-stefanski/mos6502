#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Mos6502.hpp"
#include "Bus/ArrayMemory.hpp"
#include "Bus/InstructionMemory.hpp"

constexpr Addr program_start{0x8090};

class IndirectYInstructions : public Mos6502
{
public:
  inline static constexpr Addr MemoryPage{0xa000};
  inline static constexpr Addr PointerTable{0x0020};
  inline static constexpr Addr RamAddr{0x2000};

  IndirectYInstructions() : Mos6502(Mos6502::JumpVector{.reset = program_start})
  {
    std::vector< u8 > romData;
    romData.reserve(0x100);
    for (u16 i = 0; i < 0x100; ++i)
      romData.push_back(i & 0xFF);

    bus->insert_device(
      MemoryPage,
      std::unique_ptr< BusDevice >(new ArrayMemory(
        "Custom Memory",
        std::move(romData)
      ))
    );

    bus->insert_device(
      RamAddr,
      std::unique_ptr< BusDevice >(new ArrayMemory(
        "RAM",
        std::vector< u8 >(512)
      ))
    );

    std::vector< Addr > pointers;
    for (u16 i = 0; i < 0x40; i++)
      pointers.emplace_back(MemoryPage + i);

    auto pointer_object = ArrayMemory("Pointer Table", std::move(pointers));
    for (size_t i = 0; i < pointer_object.size(); i++)
      bus->set< u8 >(PointerTable + i, pointer_object.get< u8 >(i));
  }

  inline static constexpr Addr TestProgramStart{program_start + 9};
  void LoadRegisters()
  {
    constexpr u8 lda_val{0xC2};
    constexpr u8 ldx_val{0x21};
    constexpr u8 ldy_val{0x37};

    bus->insert_device(
      program_start,
      std::unique_ptr< BusDevice >(new InstructionMemory(
        "Program Memory",
        {
          Instruction::absolute(AbsoluteOpcodes::LDA, MemoryPage + lda_val),
          Instruction::absolute(AbsoluteOpcodes::LDX, MemoryPage + ldx_val),
          Instruction::absolute(AbsoluteOpcodes::LDY, MemoryPage + ldy_val),
        }
      ))
    );

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
  bus->insert_device(
    program_start,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::indirect(IndirectYOpcodes::LDA, PointerTable),
        Instruction::nop()
      }
    ))
  );

  reset_to_entry();
  ASSERT_EQ(cpu->PC, program_start + 2); // LDX instruction, load indirect low

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
  bus->insert_device(
    TestProgramStart,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::indirect(IndirectYOpcodes::STA, PointerTable),
        Instruction::nop()
      }
    ))
  );

  LoadRegisters();

  {
    SCOPED_TRACE("STA load zero-page offset");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
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
    expect_bus_read(TestProgramStart + 2);
  }
}

TEST_F(IndirectYInstructions, BitOpsTest)
{
  bus->insert_device(
    TestProgramStart,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::indirect(IndirectYOpcodes::ORA, PointerTable),
        Instruction::indirect(IndirectYOpcodes::AND, PointerTable),
        Instruction::indirect(IndirectYOpcodes::EOR, PointerTable),
        Instruction::nop()
      }
    ))
  );

  LoadRegisters();

  {
    SCOPED_TRACE("ORA load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(4);
  {
    SCOPED_TRACE("ORA perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a | *prev_state.y});
  }

  tick(5);
  {
    SCOPED_TRACE("AND perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a & *prev_state.y});
  }

  tick(5);
  {
    SCOPED_TRACE("EOR perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a ^ *prev_state.y});
  }
}

TEST_F(IndirectYInstructions, AddSbcTest)
{
  bus->insert_device(
    TestProgramStart,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::indirect(IndirectYOpcodes::ADC, PointerTable),
        Instruction::indirect(IndirectYOpcodes::SBC, PointerTable),
        Instruction::nop()
      }
    ))
  );

  LoadRegisters();

  {
    SCOPED_TRACE("ADC load addr");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  tick(4);
  {
    SCOPED_TRACE("ADC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a + *prev_state.y});
  }

  tick(5);
  {
    SCOPED_TRACE("SBC perform op");
    expect_regs_change({.pc = NEXT_PC, .a = *prev_state.a - *prev_state.y - !*prev_flags.carry});
  }
}

TEST_F(IndirectYInstructions, CmpTest)
{
  bus->insert_device(
    TestProgramStart,
    std::unique_ptr< BusDevice >(new InstructionMemory(
      "Program Memory",
      {
        Instruction::indirect(IndirectYOpcodes::CMP, PointerTable),
        Instruction::nop()
      }
    ))
  );

  LoadRegisters();

  {
    SCOPED_TRACE("CMP Load zero-page offset");
    expect_regs_change({.pc = NEXT_PC});
    expect_bus_read(TestProgramStart + 1);
  }

  // Load address from zero-page.
  tick(2);

  tick();
  {
    SCOPED_TRACE("CMP Load value pointed by zero-page addr");
    expect_regs_change({});
    expect_bus_read(MemoryPage + *prev_state.y);
  }

  tick(2);
  {
    SCOPED_TRACE("CMP perform compare");
    expect_flags_change({});
  }
}
