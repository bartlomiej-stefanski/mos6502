#pragma once

#include <concepts>
#include <cstddef>
#include <string>

template< typename T >
concept BusElement = requires(T t) {
  { t.get_name() } -> std::convertible_to< std::string >;
  { t.size() } -> std::convertible_to< size_t >;
};
