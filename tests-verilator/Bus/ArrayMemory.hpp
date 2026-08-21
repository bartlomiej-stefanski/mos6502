#pragma once

#include <algorithm>
#include <span>
#include <vector>

#include "Bus/BusMemory.hpp"
#include "Types.hpp"

template< typename T >
struct ArrayMemory : BusMemory, std::vector< T >
{
  ArrayMemory(const std::string& name, std::vector< T >&& data)
    : BusMemory(name), std::vector< T >(std::move(data))
  {
  }

  void get_data(size_t offset, std::span< u8 > data) override {
    u8* raw_data{reinterpret_cast< u8* >(this->data())};
    auto src_begin = raw_data + offset;
    auto src_end = src_begin + data.size();
    std::copy(src_begin, src_end, data.begin());
  }

  void set_data(size_t offset, std::span< const u8 > data) override {
    u8* raw_data{reinterpret_cast< u8* >(this->data())};
    std::copy(data.begin(), data.end(), raw_data + offset);
  }

  size_t size() override { return std::vector< T >::size() * sizeof(T); };
};
