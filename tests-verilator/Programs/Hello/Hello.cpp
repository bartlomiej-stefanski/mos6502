#include <gtest/gtest.h>

#include <VtopEntity.h>
#include <memory>

#include "CpuTest.hpp"
#include "MemoryArea.hpp"
#include "Types.hpp"

constexpr Addr RomStart{0xE000};
constexpr Addr RamStart{0x0200};
constexpr Addr Serial{0x8000};

class Hello : public CpuTest
{
protected:
  void SetUpMemory() override
  {
    CpuTest::SetUpMemory();

    memory_maps.insert({
      RomStart,
      std::unique_ptr< MemoryArea >(new MemoryMappedBinary(
        "ResetVector", ROM_BINARIES_DIR "/hello/hello.bin"
      ))
    });

    memory_maps.insert({
      RamStart,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "Ram", std::vector< u8 >(0x7E00)
      ))
    });

    memory_maps.insert({
      Serial,
      std::unique_ptr< MemoryArea >(new MemoryObject(
        "Serial", std::vector< u8 >(0x100)
      ))
    });
  }
};

TEST_F(Hello, ShouldWriteHello)
{
  reset_to_entry();

  tick(5000); // Should be enough for C-runtime init and execution.

  static constexpr char hello_string[] = "hello";

  for (size_t i = 0; i < sizeof(hello_string); i++) {
    EXPECT_EQ(hello_string[i], get_memory(Serial + i, false));
  }
}
