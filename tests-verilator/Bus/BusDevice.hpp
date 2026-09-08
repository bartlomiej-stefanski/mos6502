#pragma once

#include <concepts>
#include <cstddef>
#include <string>

#include "Types.hpp"

class BusDevice
{
public:
  BusDevice(const std::string& name) : name(name) {}
  virtual ~BusDevice() = default;

  template< std::integral TI >
  TI get(size_t offset) {
    if constexpr (sizeof(TI) == sizeof(u8)) {
      return static_cast< TI >(get_u8(offset));
    } else if constexpr (sizeof(TI) == sizeof(u16)) {
      return static_cast< TI >(get_u16(offset));
    } else if constexpr (sizeof(TI) == sizeof(u32)) {
      return static_cast< TI >(get_u32(offset));
    } else if constexpr (sizeof(TI) == sizeof(u64)) {
      return static_cast< TI >(get_u64(offset));
    } else {
      static_assert(sizeof(TI) <= 8, "Unsupported integral type size.");
    }
  }

  virtual u8 get_u8(size_t offset) = 0;
  virtual u16 get_u16(size_t offset) = 0;
  virtual u32 get_u32(size_t offset) = 0;
  virtual u64 get_u64(size_t offset) = 0;

  template< std::integral TI >
  void set(size_t offset, TI value) {
    if constexpr (sizeof(TI) == sizeof(u8)) {
      set_u8(offset, static_cast< u8 >(value));
    } else if constexpr (sizeof(TI) == sizeof(u16)) {
      set_u16(offset, static_cast< u16 >(value));
    } else if constexpr (sizeof(TI) == sizeof(u32)) {
      set_u32(offset, static_cast< u32 >(value));
    } else if constexpr (sizeof(TI) == sizeof(u64)) {
      set_u64(offset, static_cast< u64 >(value));
    } else {
      static_assert(sizeof(TI) <= 8, "Unsupported integral type size.");
    }
  }

  virtual void set_u8(size_t offset, u8 value) = 0;
  virtual void set_u16(size_t offset, u16 value) = 0;
  virtual void set_u32(size_t offset, u32 value) = 0;
  virtual void set_u64(size_t offset, u64 value) = 0;

  virtual size_t size() = 0;

  const std::string& get_name() const { return name; };

protected:
  std::string name;
};
