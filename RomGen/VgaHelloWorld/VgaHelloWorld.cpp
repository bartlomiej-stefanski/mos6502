#include <algorithm>
#include <unordered_map>

#include "MemoryLayer.hpp"
#include "Instructions.hpp"
#include "WrtieMemory.hpp"

int main()
{
  constexpr Addr VgaRegion{0x6000};
  constexpr Addr VgaMemoryStart{0xa000};

  constexpr Addr VgaPointer{0xf000};
  constexpr Addr VgaMemPointer{0xf002};

  constexpr Addr CodeStart{0xe000};

  std::string vga_hello_line0 = "  Hello World MOS6502!    ";
  std::string vga_hello_line1 = "    ~ design by Bartlomiej Stefanski    ";

  std::vector< u8 > vga_line0(80, 0);
  std::copy(vga_hello_line0.begin(), vga_hello_line0.end(), vga_line0.begin());

  std::vector< u8 > vga_line1(80, 0);
  std::copy(vga_hello_line1.begin(), vga_hello_line1.end(), vga_line1.begin());

  std::vector< u8 > vga_mem{std::move(vga_line0)};
  vga_mem.insert(vga_mem.end(), vga_line1.begin(), vga_line1.end());


  std::vector< Instruction > vga_loader{
    Instruction::immediate(ImmediateOpcodes::LDY, 0),
    Instruction::indirect(IndirectYOpcodes::LDA, VgaMemPointer),
    Instruction::indirect(IndirectYOpcodes::STA, VgaPointer),
    Instruction::inner(InnerStateOpcodes::INY),
    Instruction::jumpAbsolute(CodeStart + 2),
  };


  std::unordered_map< Addr, MemoryLayer > code_rom = {
    {VgaMemoryStart, MemoryLayer("Vga Memory", std::move(vga_mem))},

    {VgaMemPointer, MemoryLayer("Vga Memory Ptr", {
      MO(VgaMemoryStart)
    })},
    {VgaPointer, MemoryLayer("Vga Ptr", {
      MO(VgaRegion)
    })},

    {ResetVector, MemoryLayer("ResetVector", {
      MO(CodeStart)
    })},

    {CodeStart, MemoryLayer("ProgramCode", std::move(vga_loader))}
  };

  write_memory(std::move(code_rom));
}
