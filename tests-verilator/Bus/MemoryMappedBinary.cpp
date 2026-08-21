#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cerrno>
#include <cstring>
#include <format>
#include <stdexcept>

#include "Bus/MemoryMappedBinary.hpp"
#include "Logger.hpp"

MemoryMappedBinary::MemoryMappedBinary(const std::string& name, i32 fd, size_t offset)
  : BusMemory(name), fd(fd), file_offset(offset)
{
  map_file_to_memory(fd);
}

MemoryMappedBinary::MemoryMappedBinary(const std::string& name, const std::string& filename, size_t offset)
  : BusMemory(name), file_offset(offset)
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

  INFO("MemoryMappedBinary: Loaded file '{}' with size: 0x{:x}", name, file_size);

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

void MemoryMappedBinary::get_data(size_t offset, std::span< u8 > data) {
  std::copy(file_mapping + offset, file_mapping + offset + data.size(), data.begin());
}

void MemoryMappedBinary::set_data(size_t offset, std::span< const u8 > data) {
  std::copy(data.begin(), data.end(), file_mapping + offset);
}
