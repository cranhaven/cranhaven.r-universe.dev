#pragma once
#ifndef __HILBERT_TYPES_H__
#define __HILBERT_TYPES_H__

#include <type_traits>
#include <vector>

using std::vector;
using std::enable_if;
using std::is_integral;
using std::is_floating_point;

namespace hilbert
{

namespace types
{

template <typename T>
using integral_vector = typename enable_if<is_integral<T>::value, vector<T>>::type;

template <typename T>
using numeric_vector = typename enable_if<is_integral<T>::value | is_floating_point<T>::value, vector<T>>::type;

template <typename T>
using integral_void = typename enable_if<is_integral<T>::value>::type;

} // namespace types

} // namespace hilbert

#endif
