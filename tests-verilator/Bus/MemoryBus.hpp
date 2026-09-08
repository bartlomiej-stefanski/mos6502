#pragma once

#include <concepts>
#include <functional>
#include <map>
#include <memory>
#include <optional>

#include "Bus/BusDevice.hpp"
#include "Bus/BusHelpers.hpp"
#include "Logger.hpp"


// TODO: Add endianness support.
struct MemoryBusConfig {
  bool allow_unaligned_accesses{false};
  bool allow_unmapped_read{false};
  bool allow_unmapped_write{false};
  bool allow_multi_device_accesses{false};
  bool trace_all_accesses{false};
};


template< std::unsigned_integral TAddr >
class MemoryBus
{
public:
  using OptionalBusDeviceRef = std::optional< std::pair< std::reference_wrapper< BusDevice >, size_t > >;

  MemoryBus(MemoryBusConfig config) : config(config) {};

  template< std::integral TI >
  TI get(TAddr addr) {
    validate_bus_offset< TI >(addr);
    OptionalBusDeviceRef device{get_device< TI >(addr)};
    if (device) {
      auto& [dev, offset]{*device};
      const auto read_value{dev.get().template get< TI >(offset)};
      if (config.trace_all_accesses) {
        INFO("MemoryBus: Read at address 0x{:x} value 0x{:x} from device '{}'.", addr, read_value, dev.get().get_name());
      }
      return read_value;
    } else {
      WARNING("MemoryBus: Unmapped reads are not allowed 0x{:x}.", addr);
      if (!config.allow_unmapped_read) {
        throw std::runtime_error("MemoryBus: Unmapped reads are not allowed.");
      } else {
        return TI{};
      }
    }
  }

  template< std::integral TI >
  void set(TAddr addr, TI value) {
    validate_bus_offset< TI >(addr);
    OptionalBusDeviceRef device{get_device< TI >(addr)};
    if (device) {
      auto& [dev, offset]{*device};
      if (config.trace_all_accesses) {
        INFO("MemoryBus: Write at address 0x{:x} with value 0x{:x} to device '{}'.", addr, value, dev.get().get_name());
      }
      dev.get().template set< TI >(offset, value);
    } else {
      WARNING("MemoryBus: Unmapped write at address 0x{:x} with value 0x{:x}.", addr, value);
      if (!config.allow_unmapped_write) {
        throw std::runtime_error("MemoryBus: Unmapped writes are not allowed.");
      }
    }
  }

  void insert_device(TAddr addr, std::unique_ptr< BusDevice > bus_device) {
    // TODO: Check if new device does not cover existing ones.
    const auto addr_device{memory_map.find(addr)};
    if (addr_device != memory_map.end()) {
      throw std::runtime_error(std::format("MemoryBus: Device '{}' already exists at address 0x{:x}.", addr_device->second->get_name(), addr));
    }

    memory_map.insert({addr, std::move(bus_device)});
  }

private:
  MemoryBusConfig config;

  std::map< TAddr, std::unique_ptr< BusDevice > > memory_map;

  template< std::integral TI >
  void validate_bus_offset(TAddr addr) {
    if (!is_aligned(addr, sizeof(TI))) {
      WARNING("MemoryBus: Unaligned memory access at address 0x{:x}.", addr);
      if (!config.allow_unaligned_accesses) {
        throw std::runtime_error("MemoryBus: Unaligned memory access not allowed.");
      }
    }
  }

  template< std::integral TI >
  OptionalBusDeviceRef get_device(const TAddr addr){
    auto upper_bound{memory_map.upper_bound(addr)};
    if (upper_bound != memory_map.begin()) {
      auto& [dev_addr, dev]{*(--upper_bound)};
      const auto offset{static_cast< size_t >(addr - dev_addr)};

      const auto fits_in_device{offset + sizeof(TI) <= dev->size()};
      if (!fits_in_device) {
        WARNING("MemoryBus: Access at address 0x{:x} with size {} does not fit in device '{}'.", addr, sizeof(TI), dev->get_name());
        if (!config.allow_multi_device_accesses) {
          throw std::runtime_error("MemoryBus: Access does not fit in device.");
        }
      }

      if (offset < dev->size()) {
        return OptionalBusDeviceRef({*dev, offset});
      }
    }

    return OptionalBusDeviceRef{};
  }
};
