#pragma once

#include <unordered_map>

#include "MemoryLayer.hpp"

constexpr Addr CodeRomStart{0x8000};
constexpr u64 AddressSpaceSize{0x10000};

void print_u8(u8 val);

void write_memory(std::unordered_map< Addr, MemoryLayer >&& code_rom);
