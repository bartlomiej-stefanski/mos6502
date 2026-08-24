#pragma once

#include <print>
#include <format>

#ifndef LOG_LEVEL
#define LOG_LEVEL 1
#endif

#if (LOG_LEVEL >= 0)
#define ERROR(message, ...) {std::print(stderr, "[ERROR] {}\n", std::format(message __VA_OPT__(,) __VA_ARGS__)); }
#else
#define ERROR(message, ...)
#endif

#if (LOG_LEVEL >= 1)
#define WARNING(message, ...) {std::print(stderr, "[WARNING] {}\n", std::format(message __VA_OPT__(,) __VA_ARGS__)); }
#else
#define WARNING(message, ...)
#endif

#if (LOG_LEVEL >= 2)
#define INFO(message, ...) {std::print(stderr, "[INFO] {}\n", std::format(message __VA_OPT__(,) __VA_ARGS__)); }
#else
#define INFO(message, ...)
#endif

#if (LOG_LEVEL >= 3)
#define NOISY(message, ...) {std::print(stderr, "[NOISY] {}\n", std::format(message __VA_OPT__(,) __VA_ARGS__)); }
#else
#define NOISY(message, ...)
#endif
