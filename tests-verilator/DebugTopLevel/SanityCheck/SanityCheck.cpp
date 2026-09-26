#include <gtest/gtest.h>

#include <VtopEntity.h>

#include "Mos6502.hpp"
#include "Bus/ArrayMemory.hpp"

TEST(SanityCheck, AmISane)
{
  VtopEntity cpu;
}

constexpr Addr MagicAddressOne{1234};
constexpr Addr MagicAddressTwo{12};

class SanityCheckBus : public Mos6502
{
public:
  SanityCheckBus() {
    bus->insert_device(
      MagicAddressOne,
      std::unique_ptr< BusDevice >(new ArrayMemory< u8 >(
        "Sequence one",
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
      ))
    );

    bus->insert_device(
      MagicAddressTwo,
      std::unique_ptr< BusDevice >(new ArrayMemory< u8 >(
        "Sequence two",
        {90, 89, 88, 87, 86, 85, 84, 83, 82, 81}
      ))
    );
  }
};

TEST_F(SanityCheckBus, ReadCheck)
{
  for (u64 i{0}; i < 10; i++) {
    EXPECT_EQ(bus->get< u8 >(MagicAddressOne + i), i);
  }

  for (u64 i{0}; i < 10; i++) {
    EXPECT_EQ(bus->get< u8 >(MagicAddressTwo + i), 90 - i);
  }
}

TEST_F(SanityCheckBus, WriteCheck)
{
  for (u64 i{0}; i < 10; i++) {
    bus->set< u8 >(MagicAddressOne + i, 50 - i);
  }

  for (u64 i{0}; i < 10; i++) {
    EXPECT_EQ(bus->get< u8 >(MagicAddressOne + i), 50 - i);
  }
}
