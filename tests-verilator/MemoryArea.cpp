#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cerrno>
#include <cstring>
#include <format>
#include <stdexcept>

#include "MemoryArea.hpp"
#include "Logger.hpp"

MemoryObject::MemoryObject(const std::string& name, std::vector< MemoryOccupant >&& data)
  : MemoryArea(name)
{
  for (auto& element: data) {
    if (std::holds_alternative< Addr >(element)) {
      const Addr addr{std::get< Addr >(element)};
      this->data.push_back(addr & 0xFF);
      this->data.push_back(addr >> 8);
    }
    else if (std::holds_alternative< u8 >(element)) {
      const u8 value{std::get< u8 >(element)};
      this->data.push_back(value);
    }
    else if (std::holds_alternative< Instruction >(element)) {
      const Instruction instr{std::get< Instruction >(element)};
      const auto bytes{instr.to_bytes()};
      this->data.insert(this->data.end(), bytes.begin(), bytes.end());
    }
  }
}

MemoryObject::MemoryObject(const std::string& name, std::vector< u8 >&& data)
  : MemoryArea(name), data(std::move(data))
{
}

MemoryObject::MemoryObject(const std::string& name, std::vector< Instruction >&& data)
  : MemoryArea(name)
{
  this->name = name;
  for (auto& instr: data) {
    const auto bytes{instr.to_bytes()};
    this->data.insert(this->data.end(), bytes.begin(), bytes.end());
  }
}

MemoryMappedBinary::MemoryMappedBinary(const std::string& name, i32 fd, size_t offset)
  : MemoryArea(name), fd(fd), file_offset(offset)
{
  map_file_to_memory(fd);
}

MemoryMappedBinary::MemoryMappedBinary(const std::string& name, const std::string& filename, size_t offset)
  : MemoryArea(name), file_offset(offset)
{
  fd = open(filename.c_str(), O_RDONLY);
  if (fd < 0) {
    throw std::runtime_error(std::format(
      "MemoryMappedBinary: failed to open file '{}': {}",
      filename,
      std::strerror(errno)
    ));
  }

  try {
    map_file_to_memory(fd);
  }
  catch (...) {
    close(fd);
    throw;
  }
}

MemoryMappedBinary::~MemoryMappedBinary()
{
  munmap(file_mapping, file_size);
  close(fd);
}

void MemoryMappedBinary::map_file_to_memory(int fd)
{
  struct stat file_info;
  if (fstat(fd, &file_info) != 0) {
    throw std::runtime_error(std::format(
      "MemoryMappedBinary: fstat failed: {}",
      std::strerror(errno)
    ));
  }

  file_size = static_cast< size_t >(file_info.st_size);
  if (file_size == 0) {
    throw std::runtime_error("MemoryMappedBinary: file is empty.");
  }

  if (file_offset > file_size) {
    throw std::runtime_error(std::format("MemoryMappedBinary: offset {} is beyond file size {}.", file_offset, file_size));
  }

  INFO("Loaded file '{}' with size: 0x{:x}", name, file_size);

  file_mapping = static_cast< u8* >(mmap(
    nullptr,
    file_size,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE,
    fd,
    0
  ));
  if (file_mapping == MAP_FAILED) {
    throw std::runtime_error(std::format(
      "MemoryMappedBinary: mmap failed: {}",
      std::strerror(errno)
    ));
  }
}
