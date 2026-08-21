#include <string>

#include "Bus/BusMemory.hpp"
#include "Types.hpp"

struct MemoryMappedBinary : BusMemory
{
  MemoryMappedBinary(const std::string& name, i32 fd, size_t offset = 0);
  MemoryMappedBinary(const std::string& name, const std::string& filename, size_t offset = 0);
  ~MemoryMappedBinary() override;

  void get_data(size_t offset, std::span< u8 > data) override;
  void set_data(size_t offset, std::span< const u8 > data) override;

  size_t size() override { return file_size - file_offset; };

private:
  i32 fd;
  u8* file_mapping;

  size_t file_size;
  size_t file_offset;

  void map_file_to_memory(int fd);
};
