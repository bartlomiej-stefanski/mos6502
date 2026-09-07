#include <stdint.h>


unsigned char to_color(uint8_t r, uint8_t g, uint8_t b) {
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
  for (h = 0; h < H_PIXELS; h++) {
    for (v = 0; v < V_PIXELS; v++) {
      *(get_pixel(h, v)) =to_color(v & 0x7, h & 0x7, 0x3);
    }
  }

  return 0;
}
