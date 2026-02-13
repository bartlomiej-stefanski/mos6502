#include <unordered_map>

#include "MemoryLayer.hpp"
#include "Instructions.hpp"
#include "WrtieMemory.hpp"

int main()
{
  constexpr Addr CodeStart{0xe000};
  constexpr Addr MainStart{CodeStart + 0x50};

  constexpr Addr SwitchAddr{0x4002};
  constexpr Addr buttonAddr{0x4003};

  constexpr Addr LedAddr{0x4000};
  constexpr Addr SegAddr{0x4001};

  std::unordered_map< Addr, MemoryLayer > code_rom = {

    {ResetVector, MemoryLayer("ResetVector", {
      MO(CodeStart)
    })},

    {CodeStart, MemoryLayer("ProgramCode", std::vector< Instruction >{

      Instruction::absolute(AbsoluteOpcodes::LDA, SwitchAddr),
      Instruction::absolute(AbsoluteOpcodes::STA, LedAddr),

      Instruction::absolute(AbsoluteOpcodes::INC, 0x50),

      Instruction::stack(StackOpcodes::PHP),
      Instruction::zero_page(ZeroPageOpcodes::LDA, 0x51),
      Instruction::stack(StackOpcodes::PLP),
      Instruction::immediate(ImmediateOpcodes::ADC, 0), // + Carry from INC
      Instruction::zero_page(ZeroPageOpcodes::STA, 0x51),

      Instruction::stack(StackOpcodes::PHP),
      Instruction::zero_page(ZeroPageOpcodes::LDA, 0x52),
      Instruction::stack(StackOpcodes::PLP),
      Instruction::immediate(ImmediateOpcodes::ADC, 0), // + Carry from INC

      Instruction::absolute(AbsoluteOpcodes::STA, SegAddr),

      Instruction::absolute(AbsoluteOpcodes::CMP, SwitchAddr),
      Instruction::branch(BranchOpcodes::BNE, 2),
        Instruction::immediate(ImmediateOpcodes::LDA, 0),

      Instruction::zero_page(ZeroPageOpcodes::STA, 0x52),

      Instruction::jumpAbsolute(CodeStart),
    })}
  };

  write_memory(std::move(code_rom));
}
