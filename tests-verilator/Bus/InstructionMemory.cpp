#include <algorithm>
#include <span>
#include <vector>

#include "Bus/InstructionMemory.hpp"
#include "Bus/BusMemory.hpp"

InstructionMemory::InstructionMemory(const std::string& name, std::vector< Instruction >&& data)
  : BusMemory(name)
{
  this->data.reserve(data.size());
  for (auto& instr: data) {
    const auto bytes{instr.to_bytes()};
    this->data.insert(this->data.end(), bytes.begin(), bytes.end());
  }
}

void InstructionMemory::get_data(size_t offset, std::span< u8 > data) {
  auto src_begin = this->data.begin() + offset;
  auto src_end = src_begin + data.size();
  std::copy(src_begin, src_end, data.begin());
}

void InstructionMemory::set_data(size_t offset, std::span< const u8 > data) {
  std::copy(data.begin(), data.end(), this->data.begin() + offset);
}
