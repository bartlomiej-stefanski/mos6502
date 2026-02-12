#include <iostream>
#include <bitset>

#include "Font.hpp"
#include "WrtieMemory.hpp"

int main() {
  for (size_t i = 0; i < sizeof(font_blob); i++) {
    print_u8(font_blob[i]);
  }
}
