#pragma once

#include <print>
#include <format>

#ifndef LOG_LEVEL
#define LOG_LEVEL 1
#endif

#if (LOG_LEVEL >= 0)
#define ERROR(message, ...) {std::print(stderr, "[ERROR] %s\n", std::format(message,  ##__VA_ARGS__)); }
#else
#define ERROR(message, ...)
#endif

#if (LOG_LEVEL >= 1)
#define WARNING(message, ...) {std::print(stderr, "[WARNING] %s\n", std::format(message,  ##__VA_ARGS__)); }
#else
#define WARNING(message, ...)
#endif

#if (LOG_LEVEL >= 2)
#define ERROR(message, ...) {std::print(stderr, "[ERROR] %s\n", std::format(message,  ##__VA_ARGS__)); }
#else
#define INFO(message, ...)
#endif
