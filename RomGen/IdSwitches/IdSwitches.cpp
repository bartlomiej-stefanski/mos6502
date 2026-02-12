#include <unordered_map>

#include "MemoryLayer.hpp"
#include "Instructions.hpp"
#include "WrtieMemory.hpp"

int main()
{
  constexpr Addr CodeStart{0xe000};
  constexpr Addr SwitchAddr{0x4000};
  constexpr Addr LedAddr{0x4001};

  std::unordered_map< Addr, MemoryLayer > code_rom = {

    {ResetVector, MemoryLayer("ResetVector", {
      MO(CodeStart)
    })},

    {CodeStart, MemoryLayer("ProgramCode", std::vector< Instruction >{
      Instruction::absolute(AbsoluteOpcodes::LDA, SwitchAddr),
      Instruction::absolute(AbsoluteOpcodes::STA, LedAddr),
      Instruction::jumpAbsolute(CodeStart)
    })}
  };

  write_memory(std::move(code_rom));
}
