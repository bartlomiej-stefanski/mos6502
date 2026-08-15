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

  u8& at(size_t inx) override { return data.at(inx); }
  size_t size() override { return data.size(); };

private:
  std::vector< u8 > data;
};

struct MemoryMappedBinary : MemoryArea
{
  MemoryMappedBinary(const std::string& name, i32 fd, size_t offset = 0);
  MemoryMappedBinary(const std::string& name, const std::string& filename, size_t offset = 0);
  ~MemoryMappedBinary() override;

  u8& at(size_t inx) override { return file_mapping[inx + file_offset]; }
  size_t size() override { return file_size - file_offset; };

private:
  i32 fd;
  u8* file_mapping;

  size_t file_size;
  size_t file_offset;

  void map_file_to_memory(int fd);
};
