#include <bitset>
#include <iostream>

#include "WrtieMemory.hpp"
#include "Types.hpp"

void print_u8(u8 val)
{
  std::bitset< 8 > bits{val};
  for (u8 i = 0; i < 8; i++) {
    std::cout << (bits[7 - i] ? "1" : "0");
  }

  std::cout << '\n';
}

void write_memory(std::unordered_map< Addr, MemoryLayer >&& code_rom)
{
  u64 code_pos = CodeRomStart;

  while (code_pos < AddressSpaceSize) {
    if (code_rom.contains(code_pos)) {
      const auto& layer{code_rom.at(code_pos)};
      for (u64 inx = 0; inx < layer.size() && code_pos < AddressSpaceSize; inx++) {
        print_u8(layer[inx]);
        code_pos++;
      }
    }
    else {
      print_u8(0);
      code_pos++;
    }
  }
}
