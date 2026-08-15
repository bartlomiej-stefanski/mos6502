#pragma once

#include <vector>
#include <variant>
#include <string>

#include "Types.hpp"
#include "Instructions.hpp"

using MemoryOccupant = std::variant< Addr, u8, Instruction >;
using MO = MemoryOccupant;

struct MemoryArea
{
  MemoryArea(const std::string& name) : name(name) {}

  virtual size_t size() = 0;
  virtual u8& at(size_t inx) = 0;

  const std::string& get_name() { return name; };

  virtual ~MemoryArea() = default;

  std::string name;
};

struct MemoryObject : MemoryArea
{
  MemoryObject(const std::string& name, std::vector< MemoryOccupant >&& data);
  MemoryObject(const std::string& name, std::vector< u8 >&& data);
  MemoryObject(const std::string& name, std::vector< Instruction >&& data);

  u8 & at(size_t inx) override { return data.at(inx); }
  size_t size() override { return data.size(); };

private:
  std::vector< u8 > data;
};
