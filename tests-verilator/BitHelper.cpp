#include "BitHelper.hpp"

u8 ror(u8 x, bool carry_flag)
{
  return (x >> 1) | (carry_flag << 7);
}

u8 rol(u8 x, bool carry_flag)
{
  return (x << 1) | carry_flag;
}
