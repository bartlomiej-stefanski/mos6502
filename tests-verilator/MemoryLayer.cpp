#include "MemoryLayer.hpp"

MemoryLayer::MemoryLayer(const std::string name, std::vector< MemoryOccupant >&& data)
  : std::vector< u8 >(), name(name)
{
  for (auto& element: data) {
    if (std::holds_alternative< Addr >(element)) {
      const Addr addr{std::get< Addr >(element)};
      this->push_back(addr & 0xFF);
      this->push_back(addr >> 8);
    }
    else if (std::holds_alternative< u8 >(element)) {
      const u8 value{std::get< u8 >(element)};
      this->push_back(value);
    }
    else if (std::holds_alternative< Instruction >(element)) {
      const Instruction instr{std::get< Instruction >(element)};
      const auto bytes{instr.to_bytes()};
      this->insert(this->end(), bytes.begin(), bytes.end());
    }
  }
}

MemoryLayer::MemoryLayer(const std::string name, std::vector< u8 >&& data)
  : std::vector< u8 >(data), name(name)
{
}

MemoryLayer::MemoryLayer(const std::string name, std::vector< Instruction >&& data)
  : std::vector< u8 >(), name(name)
{
  for (auto& instr: data) {
    const auto bytes{instr.to_bytes()};
    this->insert(this->end(), bytes.begin(), bytes.end());
  }
}
