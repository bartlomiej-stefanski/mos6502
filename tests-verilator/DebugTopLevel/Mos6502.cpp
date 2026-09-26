#include <vector>
#include <print>
#include <format>

#include <gtest/gtest.h>

#include "Bus/ArrayMemory.hpp"
#include "Bus/BusDevice.hpp"
#include "Instructions.hpp"
#include "Types.hpp"
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

Mos6502::~Mos6502() {
  if (log_output) {
    log_output->close();
  }
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

  const auto decoding_opcode{cpu->OPCODE_ON_BUS || cpu->OPCODE_ON_LATCH};
  const u8 opcode{cpu->OPCODE_ON_BUS ? cpu->MEM_DATA_IN : cpu->LATCH};

  cpu->eval();
  cpu->CLK = 1;
  cpu->eval();

  if (decoding_opcode) {
    const OpCodeInfo opcode_info{Instruction::get_opcode_info(opcode)};
    const auto opcode_text{get_opcode_text(opcode_info, cpu->PC - 1)};
    const auto flags_text{get_flag_text()};
    const auto message{std::format("{}, 0x{:02x}, 0x{:02x}, 0x{:02x}, 0x{:02x}, {}", opcode_text, cpu->PC, cpu->REG_A, cpu->REG_X, cpu->REG_Y, flags_text)};
    NOISY("{}", message);
    if (log_output) {
      std::print(log_output.value(), "{}\n", message);
    }
  }
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

std::string Mos6502::get_flag_text()
{
  std::string msg{};

  if (cpu->NEG_AF) msg.push_back('N');
  if (cpu->ZERO_AF) msg.push_back('Z');
  if (cpu->CARRY_AF) msg.push_back('C');
  if (cpu->OVF_AF) msg.push_back('V');
  if (cpu->DEC_AF) msg.push_back('D');
  if (cpu->BRK_F) msg.push_back('B');
  if (cpu->INT_F) msg.push_back('I');

  return msg.empty() ? "-" : msg;
}

std::string Mos6502::get_opcode_text(const OpCodeInfo& opcode_info, Addr next_pc)
{
  uint16_t target_addr;

  switch (opcode_info.mode) {
    case IMP:
      return std::format("{}", opcode_info.mnemonic);
    case ACC:
      return std::format("{} A", opcode_info.mnemonic);
    case IMM:
      return std::format("{} 0x{:02x}", opcode_info.mnemonic, bus->get< u8 >(next_pc));
    case ZP:
      return std::format("{} $0x{:02x}", opcode_info.mnemonic, bus->get< u8 >(next_pc));
    case ZPX:
      return std::format("{} $0x{:02x}.X", opcode_info.mnemonic, bus->get< u8 >(next_pc));
    case ZPY:
      return std::format("{} $0x{:02x}.Y", opcode_info.mnemonic, bus->get< u8 >(next_pc));
    case REL:
      // Relative jump with signed parameter offset.
      target_addr = (next_pc + 1) + (int8_t)bus->get< u8 >(next_pc);
      return std::format("{} $0x{:02x}", opcode_info.mnemonic, target_addr);
    case ABS:
      return std::format("{} $0x{:02x}{:02x}", opcode_info.mnemonic, bus->get< u8 >(next_pc + 1), bus->get< u8 >(next_pc));
    case ABSX:
      return std::format("{} $0x{:02x}{:02x}.X", opcode_info.mnemonic, bus->get< u8 >(next_pc + 1), bus->get< u8 >(next_pc));
    case ABSY:
      return std::format("{} $0x{:02x}{:02x}.Y", opcode_info.mnemonic, bus->get< u8 >(next_pc + 1), bus->get< u8 >(next_pc));
    case IND:
      return std::format("{} ($0x{:02x}{:02x})", opcode_info.mnemonic, bus->get< u8 >(next_pc + 1), bus->get< u8 >(next_pc));
    case INDX:
      return std::format("{} ($0x{:02x}.X)", opcode_info.mnemonic, bus->get< u8 >(next_pc));
    case INDY:
      return std::format("{} ($0x{:02x}).Y", opcode_info.mnemonic, bus->get< u8 >(next_pc));
  }

  return "IMPOSSIBLE";
}
