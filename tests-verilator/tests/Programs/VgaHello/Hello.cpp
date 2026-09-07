#include <fstream>
#include <filesystem>
#include <memory>

#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Mos6502.hpp"
#include "Types.hpp"
#include "Bus/MemoryMappedBinary.hpp"
#include "Bus/ArrayMemory.hpp"

constexpr Addr RomStart{0xE000};
constexpr Addr RamStart{0x0200};
constexpr Addr VgaBus{0x8000};

class VgaHello : public Mos6502
{
public:
  VgaHello() {
    std::filesystem::create_directories(ARTIFACT_DIR "/Vga_Hello");
    log_output = std::ofstream(ARTIFACT_DIR "/Vga_Hello/cpu.trace", std::ios::out | std::ios::trunc);

    bus->insert_device(
      RomStart,
      std::unique_ptr< BusDevice >(new MemoryMappedBinary(
        "ROM", ROM_BINARIES_DIR "/vga_hello/vga_hello.bin"
      ))
    );

    bus->insert_device(
      RamStart,
      std::unique_ptr< BusDevice >(new ArrayMemory(
        "Ram", std::vector< u8 >(0x7E00)
      ))
    );

    bus->insert_device(
      VgaBus,
      std::unique_ptr< BusDevice >(new ArrayMemory(
        "VgaBus", std::vector< u8 >(0x3000)
      ))
    );
  }
};

#define H_PIXELS (1280 / 16)
#define V_PIXELS (720 / 16)

Addr get_pixel(uint16_t h, uint16_t v) {
  uint16_t relative_addr = (v << 7) | h;
  return VgaBus + relative_addr;
}

unsigned char to_color(uint8_t r, uint8_t g, uint8_t b) {
  return (r << 5) | (g << 2) | b;
}

TEST_F(VgaHello, ShouldWriteCheckbox)
{
  reset_to_entry();

  tick(4000000); // Should be enough for C-runtime init and execution.

  uint8_t h;
  uint8_t v;
  for (h = 0; h < H_PIXELS; h++) {
    for (v = 0; v < V_PIXELS; v++) {
      const auto pixel_color = bus->get< u8 >(get_pixel(h, v));
      ASSERT_EQ(pixel_color, to_color(v & 0x7, h & 0x7, 0x3));
    }
  }
}
