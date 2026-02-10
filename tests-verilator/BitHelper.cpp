#include "BitHelper.hpp"

u8 ror(u8 x)
{
  return (x >> 1) | ((x & 0x1) << 7);
}

u8 rol(u8 x)
{
  return (x << 1) | ((x & 0x80) >> 7);
}
