#include <gtest/gtest.h>

#include "Bus/ArrayMemory.hpp"
#include "Types.hpp"
#include "MemoryController.hpp"

MemoryController::MemoryController() {
  setup_memory();
}

MemoryController::~MemoryController() {
  if (log_output) {
    log_output->close();
  }
}

void MemoryController::setup_memory()
{
}

void MemoryController::SetUp()
{
  controller = new VmemoryController;
  controller->ENABLE = true;
}

void MemoryController::TearDown()
{
  delete controller;
}

void MemoryController::reset()
{
  controller->RESET = 1;
  tick();
  controller->RESET = 0;
  tick();
}

void MemoryController::tick()
{
  controller->CLK = 0;
  controller->eval();

  if (controller->VGA_DATA_W) {
    vga_bus.set(controller->VGA_ADDR, controller->VGA_DATA_OUT);
  }

  controller->eval();
  controller->CLK = 1;
  controller->eval();
}

void MemoryController::tick(u64 n)
{
  while (n--) {
    tick();
  }
}
