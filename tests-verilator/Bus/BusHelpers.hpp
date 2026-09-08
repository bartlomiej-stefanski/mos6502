#pragma once

#include <concepts>
#include <cstddef>
#include <map>
#include <memory>

#include "Bus/BusElement.hpp"


namespace BusHelpers {

template< std::integral Addr, BusElement Device >
using DeviceMap = std::map< Addr, std::unique_ptr< Device > >;

template< std::integral Addr >
constexpr bool is_aligned(Addr addr, size_t width) {
  return (addr % width) == 0;
}

template< std::integral Addr, BusElement BE >
BE* get_device_if_conflicts(const DeviceMap< Addr, BE >& device_map, Addr offset, Addr size)
{
  auto it{device_map.upper_bound(offset)};

  // Check previous device.
  if (it != device_map.begin()) {
    auto prev = std::prev(it);
    const auto& [prev_offset, prev_dev] = *prev;

    if (prev_offset + prev_dev->size() > offset) {
      return prev_dev.get();
    }
  }

  // Check next device.
  if (it != device_map.end()) {
    const auto& [next_offset, next_dev] = *it;

    if (offset + size > next_offset) {
      return next_dev.get();
    }
  }

  return nullptr;
}

}
