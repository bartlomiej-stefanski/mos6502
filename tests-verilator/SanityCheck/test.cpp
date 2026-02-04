#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "CpuTest.hpp"

TEST(SanityCheck, AmISane)
{
  VtopEntity cpu;
}

constexpr Addr MagicAddressOne{1234};
constexpr Addr MagicAddressTwo{12};

class SanityCheckBus : public CpuTest
{
protected:
  void SetUpMemory() override
  {
    CpuTest::SetUpMemory();

    memory_maps.insert({
      MagicAddressOne,
      MemoryLayer(
        "Sequence one",
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
      )
    });

    memory_maps.insert({
      MagicAddressTwo,
      MemoryLayer(
        "Sequence two",
        {90, 89, 88, 87, 86, 85, 84, 83, 82, 81}
      )
    });
  }
};

TEST_F(SanityCheckBus, ReadCheck)
{
  for (u64 i{0}; i < 10; i++) {
    EXPECT_EQ(get_memory(MagicAddressOne + i, false), i);
  }

  for (u64 i{0}; i < 10; i++) {
    EXPECT_EQ(get_memory(MagicAddressTwo + i, false), 90 - i);
  }
}

TEST_F(SanityCheckBus, WriteCheck)
{
  for (u64 i{0}; i < 10; i++) {
    get_memory(MagicAddressOne + i, true) = 50 - i;
  }

  for (u64 i{0}; i < 10; i++) {
    EXPECT_EQ(get_memory(MagicAddressOne + i, false), 50 - i);
  }
}
