#include <gtest/gtest.h>

#include <VtopEntity.h>
#include <memory>

#include "Mos6502.hpp"
#include "Types.hpp"
#include "Bus/MemoryMappedBinary.hpp"
#include "Bus/ArrayMemory.hpp"

constexpr Addr RomStart{0xE000};
constexpr Addr RamStart{0x0200};
constexpr Addr Serial{0x8000};

class Hello : public Mos6502
{
public:
  Hello() {
    bus->insert_device(
      RomStart,
      std::unique_ptr< BusDevice >(new MemoryMappedBinary(
        "ROM", ROM_BINARIES_DIR "/hello/hello.bin"
      ))
    );

    bus->insert_device(
      RamStart,
      std::unique_ptr< BusDevice >(new ArrayMemory(
        "Ram", std::vector< u8 >(0x7E00)
      ))
    );

    bus->insert_device(
      Serial,
      std::unique_ptr< BusDevice >(new ArrayMemory(
        "Serial", std::vector< u8 >(0x100)
      ))
    );
  }
};

TEST_F(Hello, ShouldWriteHello)
{
  reset_to_entry();

  tick(5000); // Should be enough for C-runtime init and execution.

  static constexpr char hello_string[] = "hello";

  for (size_t i = 0; i < sizeof(hello_string); i++) {
    EXPECT_EQ(hello_string[i], bus->get< u8 >(Serial + i));
  }
}
