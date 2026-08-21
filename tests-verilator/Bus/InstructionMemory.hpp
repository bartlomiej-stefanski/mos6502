#pragma once

#include <string>
#include <vector>

#include "Bus/BusMemory.hpp"
#include "Instructions.hpp"

struct InstructionMemory : BusMemory
{
  InstructionMemory(const std::string& name, std::vector< Instruction >&& data);

  void get_data(size_t offset, std::span< u8 > data) override;
  void set_data(size_t offset, std::span< const u8 > data) override;

  size_t size() override { return data.size(); };

private:
  std::vector< u8 > data;
};
