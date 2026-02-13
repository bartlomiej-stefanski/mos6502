#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Common.hpp"
#include "CpuTest.hpp"
#include "Instructions.hpp"
#include "MemoryLayer.hpp"

constexpr Addr program_start{0x8090};

class Vga : public CpuTest
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
  }
};

TEST_F(Vga, ShouldCopyMemory)
{
  constexpr Addr VgaRegion{0x6000};
  constexpr Addr VgaMemoryStart{0xa000};

  constexpr Addr VgaPointer{0xf000};
  constexpr Addr VgaMemPointer{0xf002};

  constexpr Addr SwitchAddr{0x4002};
  constexpr Addr buttonAddr{0x4003};

  constexpr Addr LedAddr{0x4000};
  constexpr Addr SegAddr{0x4001};

  std::string vga_hello_line0 = "  Hello World MOS6502!    ";
  std::string vga_hello_line1 = "    ~ design by Bartlomiej Stefanski    ";

  std::vector< u8 > vga_line0(80, 0);
  std::copy(vga_hello_line0.begin(), vga_hello_line0.end(), vga_line0.begin());

  std::vector< u8 > vga_line1(80, 0);
  std::copy(vga_hello_line1.begin(), vga_hello_line1.end(), vga_line1.begin());

  std::vector< u8 > vga_mem{std::move(vga_line0)};
  vga_mem.insert(vga_mem.end(), vga_line1.begin(), vga_line1.end());

  memory_maps.insert({
    VgaMemoryStart,
    MemoryLayer(
      "Vga Memory",
      std::move(vga_mem)
    )
  });


  memory_maps.insert({
    0x01,
    MemoryLayer(
      "First Page",
      std::vector< u8 >(1024)
    )
  });  memory_maps.insert({
    0x01,
    MemoryLayer(
      "First Page",
      std::vector< u8 >(1024)
    )
  });


  memory_maps.insert({
    program_start,
    MemoryLayer(
      "Program Memory",
      std::vector< Instruction >{
        Instruction::immediate(ImmediateOpcodes::LDY, 0),
        Instruction::indirect(IndirectYOpcodes::LDA, VgaMemPointer),
        Instruction::indirect(IndirectYOpcodes::STA, VgaPointer),
        Instruction::inner(InnerStateOpcodes::INY),
        Instruction::jumpAbsolute(program_start + 2),
      }
    )
  });

  memory_maps.insert({
    0x01,
    MemoryLayer(
      "First Page",
      std::vector< u8 >(1024)
    )
  });

  memory_maps.insert({
    SwitchAddr,
    MemoryLayer(
      "Switches",
      std::vector< u8 >{0xF0}
    )
  });

  memory_maps.insert({
    SegAddr,
    MemoryLayer(
      "First Page",
      std::vector< u8 >(4)
    )
  });

  reset_to_entry();

  tick(5 * 1000 * 1000); // 0.1s of execution at 50MHz

  const u8 m52 = get_memory(0x52, false);
  EXPECT_TRUE(m52 > 0);
}
