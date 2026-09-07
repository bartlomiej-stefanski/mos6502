#include <stdint.h>

unsigned char to_color(uint16_t h, uint16_t v) {
  uint8_t r, g, b;
  r = v & 0x7;
  g = h & 0x7;
  b = 0x1;

  return (r << 5) | (g << 2) | b;
}

#define H_PIXELS (1280 / 16)
#define V_PIXELS (720 / 16)

#define VGA_BUFFER ((volatile unsigned char*)0x8000)
volatile unsigned char* get_pixel(uint16_t h, uint16_t v) {
  uint8_t relative_addr_low = (v << 7) + h;
  uint8_t relative_addr_high = (v >> 1);
  return VGA_BUFFER + ((uint16_t)relative_addr_high << 8) + (uint16_t)relative_addr_low;
}

int main(void) {
  uint16_t h;
  uint16_t v;

  for (v = 0; v < V_PIXELS; v++) {
    for (h = 0; h < H_PIXELS; h++) {
      *(get_pixel(h, v)) = to_color(h, v);
    }
  }

  return 0;
}
