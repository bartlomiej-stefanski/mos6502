#pragma once

#include <string>
#include <span>

#include "Bus/BusDevice.hpp"

class BusMemory : public BusDevice
{
public:
  BusMemory(const std::string& name) : BusDevice(name) {}

  virtual void get_data(size_t offset, std::span< u8 > data) = 0;
  u8 get_u8(size_t offset) override;
  u16 get_u16(size_t offset) override;
  u32 get_u32(size_t offset) override;
  u64 get_u64(size_t offset) override;

  virtual void set_data(size_t offset, std::span< const u8 > data) = 0;
  void set_u8(size_t offset, u8 value) override;
  void set_u16(size_t offset, u16 value) override;
  void set_u32(size_t offset, u32 value) override;
  void set_u64(size_t offset, u64 value) override;
};
