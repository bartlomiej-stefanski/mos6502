#ifdef GEN_FONT_FILE

#include <iostream>
#include <bitset>

#include "Font.hpp"


int main() {
  for (size_t i = 0; i < sizeof(font_blob); i++) {
    std::bitset< 8 > bits = font_blob[i];
    for (size_t j = 0; j < 8; j++) {
      std::cout << (bits[7 - j] ? '1' : '0');
    }
    std::cout << '\n';
  }
}

#endif
