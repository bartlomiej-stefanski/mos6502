#include "Bus/BusMemory.hpp"

u8 BusMemory::get_u8(size_t offset) {
  u8 value;
  get_data(offset, std::span< u8 >{&value, 1});
  return value;
}

u16 BusMemory::get_u16(size_t offset) {
  u16 value;
  get_data(offset, std::span< u8 >{reinterpret_cast< u8* >(&value), sizeof(u16)});
  return value;
}
u32 BusMemory::get_u32(size_t offset) {
  u32 value;
  get_data(offset, std::span< u8 >{reinterpret_cast< u8* >(&value), sizeof(u32)});
  return value;
}
u64 BusMemory::get_u64(size_t offset) {
  u64 value;
  get_data(offset, std::span< u8 >{reinterpret_cast< u8* >(&value), sizeof(u64)});
  return value;
}

void BusMemory::set_u8(size_t offset, u8 value) {
  set_data(offset, std::span< const u8 >{&value, 1});
}
void BusMemory::set_u16(size_t offset, u16 value) {
  set_data(offset, std::span< const u8 >{reinterpret_cast< const u8* >(&value), sizeof(u16)});
}
void BusMemory::set_u32(size_t offset, u32 value) {
  set_data(offset, std::span< const u8 >{reinterpret_cast< const u8* >(&value), sizeof(u32)});
}
void BusMemory::set_u64(size_t offset, u64 value) {
  set_data(offset, std::span< const u8 >{reinterpret_cast< const u8* >(&value), sizeof(u64)});
}
