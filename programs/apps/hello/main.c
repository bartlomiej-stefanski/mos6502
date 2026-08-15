#include <stdint.h>

#define SERIAL ((volatile unsigned char*)0x8000)

int main(void) {
  const char* hello = "hello";
  uint16_t i;

  for (i = 0; i < 6; i++)
    *(SERIAL + i) = hello[i];

  return 0;
 }
