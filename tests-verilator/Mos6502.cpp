#include <vector>

#include <gtest/gtest.h>

#include "Bus/ArrayMemory.hpp"
#include "Bus/BusDevice.hpp"
#include "Mos6502.hpp"

Mos6502::Mos6502() : bus(std::make_shared< MemoryBus< Addr > >(MemoryBusConfig{})) {
  setup_memory();
}

Mos6502::Mos6502(JumpVector jump_vector) : bus(std::make_shared< MemoryBus< Addr > >(MemoryBusConfig{})) {
  setup_memory();

  bus->insert_device(NmiVector,
    std::unique_ptr< BusDevice >(new ArrayMemory< Addr >(
      "Jump Vector",
      {
        jump_vector.nmi,
        jump_vector.reset,
        jump_vector.interrupt,
      }
    ))
  );
}

void Mos6502::setup_memory()
{
  bus->insert_device(StackStart,
    std::unique_ptr< BusDevice >(new ArrayMemory(
      "Stack Memory",
      std::vector< u8 >(0x100)
    ))
  );

  bus->insert_device(0,
    std::unique_ptr< BusDevice >(new ArrayMemory(
      "Zero-page",
      std::vector< u8 >(0x100)
    ))
  );
}

void Mos6502::SetUp()
{
  cpu = new VtopEntity;
  cpu->ENABLE = true;
}

void Mos6502::TearDown()
{
  delete cpu;
}

void Mos6502::reset()
{
  cpu->RESET = 1;
  tick();
  cpu->RESET = 0;
  tick();
}

void Mos6502::tick()
{
  prev_state = {
    .pc = cpu->PC,
    .a = cpu->REG_A,
    .x = cpu->REG_X,
    .y = cpu->REG_Y,
    .sp = cpu->SP
  };

  prev_flags = {
    .carry = cpu->CARRY_AF,
    .zero = cpu->ZERO_AF,
    .interrupt_disable = cpu->INT_F,
    .decimal_mode = cpu->DEC_AF,
    .break_command = cpu->BRK_F,
    .overflow = cpu->OVF_AF,
    .negative = cpu->NEG_AF
  };

  cpu->CLK = 0;
  cpu->eval();

  const Addr mem_query{cpu->MEM_ADDR};
  const bool mem_write{cpu->MEM_W != 0};

  if (mem_write) {
    bus->set< u8 >(mem_query, cpu->MEM_W_DATA);
  } else {
    cpu->MEM_DATA_IN = bus->get< u8 >(mem_query);
  }

  cpu->eval();
  cpu->CLK = 1;
  cpu->eval();
}

void Mos6502::tick(u64 n)
{
  while (n--) {
    tick();
  }
}

void Mos6502::expect_regs_change(RegsState expected)
{
  SCOPED_TRACE("EXPECT_REGS_CHANGE");

#define CHECK_IF_CHANGE_EXPECTED(field, cpu_reg) \
  if (expected.field.has_value()) { \
    EXPECT_EQ(cpu->cpu_reg, *expected.field); \
  } \
  else { \
    EXPECT_EQ(cpu->cpu_reg, *prev_state.field); \
  }

  CHECK_IF_CHANGE_EXPECTED(pc, PC)
  CHECK_IF_CHANGE_EXPECTED(a, REG_A)
  CHECK_IF_CHANGE_EXPECTED(x, REG_X)
  CHECK_IF_CHANGE_EXPECTED(y, REG_Y)
  CHECK_IF_CHANGE_EXPECTED(sp, SP)

#undef CHECK_IF_CHANGE_EXPECTED
}

void Mos6502::expect_flags_change(CpuFlagState expected)
{
  SCOPED_TRACE("EXPECT_FLAGS_CHANGE");

#define CHECK_IF_CHANGE_EXPECTED(field, cpu_flag) \
  if (expected.field.has_value()) { \
    EXPECT_EQ(cpu->cpu_flag, *expected.field); \
  } \
  else { \
    EXPECT_EQ(cpu->cpu_flag, *prev_flags.field); \
  }

  CHECK_IF_CHANGE_EXPECTED(carry, CARRY_AF);
  CHECK_IF_CHANGE_EXPECTED(zero, ZERO_AF);
  CHECK_IF_CHANGE_EXPECTED(interrupt_disable, INT_F);
  CHECK_IF_CHANGE_EXPECTED(decimal_mode, DEC_AF);
  CHECK_IF_CHANGE_EXPECTED(break_command, BRK_F);
  CHECK_IF_CHANGE_EXPECTED(overflow, OVF_AF);
  CHECK_IF_CHANGE_EXPECTED(negative, NEG_AF);

#undef CHECK_IF_CHANGE_EXPECTED
}

void Mos6502::expect_bus_read(Addr bus_addr)
{
  SCOPED_TRACE("EXPECT_BUS_READ");

  EXPECT_EQ(cpu->MEM_ADDR, bus_addr);
  EXPECT_EQ(cpu->MEM_W, false);
}

void Mos6502::expect_bus_write(Addr bus_addr, u8 data)
{
  SCOPED_TRACE("EXPECT_BUS_WRITE");

  EXPECT_EQ(cpu->MEM_ADDR, bus_addr);
  EXPECT_EQ(cpu->MEM_W, true);
  EXPECT_EQ(cpu->MEM_W_DATA, data);
}
