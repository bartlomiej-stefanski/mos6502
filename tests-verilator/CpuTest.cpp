#include <cstdlib>
#include <iostream>
#include <format>
#include <vector>

#include <gtest/gtest.h>

#include "CpuTest.hpp"
#include "Instructions.hpp"

CpuTest::MemoryLayer::MemoryLayer(const std::string name, std::vector< MemoryOccupant >&& data)
  : std::vector< u8 >(), name(name)
{
  for (auto& element: data) {
    if (std::holds_alternative< Addr >(element)) {
      const Addr addr{std::get< Addr >(element)};
      this->push_back(addr & 0xFF);
      this->push_back(addr >> 8);
    }
    else if (std::holds_alternative< u8 >(element)) {
      const u8 value{std::get< u8 >(element)};
      this->push_back(value);
    }
    else if (std::holds_alternative< Instruction >(element)) {
      const Instruction instr{std::get< Instruction >(element)};
      const auto bytes{instr.to_bytes()};
      this->insert(this->end(), bytes.begin(), bytes.end());
    }
  }
}

CpuTest::MemoryLayer::MemoryLayer(const std::string name, std::vector< u8 >&& data)
  : std::vector< u8 >(data), name(name)
{
}

CpuTest::MemoryLayer::MemoryLayer(const std::string name, std::vector< Instruction >&& data)
  : std::vector< u8 >(), name(name)
{
  for (auto& instr: data) {
    const auto bytes{instr.to_bytes()};
    this->insert(this->end(), bytes.begin(), bytes.end());
  }
}

void CpuTest::SetUpMemory()
{
  memory_maps.insert({
    StackStart,
    MemoryLayer(
      "Stack Memory",
      std::vector< u8 >(256)
    )
  });

  memory_maps.insert({
    0,
    MemoryLayer(
      "Zero-byte",
      std::vector< u8 >(1)
    )
  });
}

void CpuTest::SetUp()
{
  cpu = new VtopEntity;

  SetUpMemory();

  cpu->ENABLE = true;
}

void CpuTest::TearDown()
{
  delete cpu;
}

void CpuTest::reset()
{
  cpu->RESET = 1;
  tick();
  cpu->RESET = 0;
  tick();
}

u8& CpuTest::get_memory(const Addr addr, const bool is_write)
{
  auto upper_bound{memory_maps.upper_bound(addr)};
  if (upper_bound != memory_maps.begin()) {
    auto& current_layer{*(--upper_bound)};
    const size_t offset{static_cast< size_t >(addr - current_layer.first)};
    if (offset < current_layer.second.size()) {
      return current_layer.second[offset];
    }
  }

  const auto message{std::format("Memory not mapped at address {}.", addr)};
  if (is_write) {
    throw UnmappedMemory(message);
  }
  else {
    std::cerr << message << '\n';
    static u8 zero = 0;
    return zero = 0;
  }
}

void CpuTest::tick()
{
  prev_state = {
    .pc = cpu->PC,
    .a = cpu->REG_A,
    .x = cpu->REG_X,
    .y = cpu->REG_Y,
    .sp = cpu->SP
  };

  cpu->CLK = 0;
  cpu->eval();

  const Addr mem_query{cpu->MEM_ADDR};
  const bool mem_w{cpu->MEM_W != 0};

  u8& mem{get_memory(mem_query, mem_w)};

  if (mem_w) {
    mem = cpu->MEM_W_DATA;
  }
  else {
    cpu->MEM_DATA_IN = mem;
  }

  cpu->eval();
  cpu->CLK = 1;
  cpu->eval();
}

void CpuTest::tick(u64 n)
{
  for (u64 i{0}; i < n; i++) {
    tick();
  }
}

void CpuTest::expect_regs_change(RegsState expected)
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

void CpuTest::expect_bus_read(Addr bus_addr)
{
  SCOPED_TRACE("EXPECT_BUS_READ");

  EXPECT_EQ(cpu->MEM_ADDR, bus_addr);
  EXPECT_EQ(cpu->MEM_W, false);
}

void CpuTest::expect_bus_write(Addr bus_addr, u8 data)
{
  SCOPED_TRACE("EXPECT_BUS_WRITE");

  EXPECT_EQ(cpu->MEM_ADDR, bus_addr);
  EXPECT_EQ(cpu->MEM_W, true);
  EXPECT_EQ(cpu->MEM_W_DATA, data);
}
