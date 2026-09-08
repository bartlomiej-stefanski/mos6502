#pragma once

#include <concepts>
#include <functional>
#include <format>
#include <map>
#include <memory>
#include <stdexcept>

#include "Bus/BusDevice.hpp"
#include "Bus/BusHelpers.hpp"

struct Register
{
  Register(const std::string& name) : name(name) {}
  virtual ~Register() = default;

  virtual u8 get_u8(size_t offset) = 0;
  virtual u16 get_u16(size_t offset) = 0;
  virtual u32 get_u32(size_t offset) = 0;
  virtual u64 get_u64(size_t offset) = 0;

  virtual void set_u8(size_t offset, u8 value) = 0;
  virtual void set_u16(size_t offset, u16 value) = 0;
  virtual void set_u32(size_t offset, u32 value) = 0;
  virtual void set_u64(size_t offset, u64 value) = 0;

  virtual size_t size() = 0;

  const std::string& get_name() const { return name; }

protected:
  std::string name;
};

class Peripheral : public BusDevice
{
public:
  Peripheral(const std::string& name) : BusDevice(name) {}

  template< std::integral I >
  void add_register(size_t offset, Register&& reg) {
    if (offset + reg.size() > size()) {
      throw std::runtime_error("Register out of bounds");
    }

    auto lower_bound{registers.lower_bound(offset)};
    if (lower_bound != registers.begin()) {
      const auto& [prev_offset, prev_reg]{*(--lower_bound)};
      const auto prev_end{prev_offset + prev_reg->size()};
      if (prev_end >= offset) {
        throw std::runtime_error(std::format("Register {} overlaps with existing register {}", reg.get_name(), prev_reg->get_name()));
      }
    }

    registers.emplace(reg);
  }

private:
  BusHelpers::DeviceMap< size_t,  Register > registers;

};
