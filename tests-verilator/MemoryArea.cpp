#include "MemoryArea.hpp"

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
