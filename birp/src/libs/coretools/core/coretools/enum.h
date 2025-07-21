/*
 * enum.h
 *
 *  Created on: Aug 16, 2022
 *      Author: Andreas
 */

#ifndef CORETOOLS_ENUM_H
#define CORETOOLS_ENUM_H

#include "coretools/Main/TError.h"
#include <type_traits>

namespace coretools {

template<typename E>
constexpr std::enable_if_t<std::is_enum_v<E>, size_t> index(E e) noexcept {
	return static_cast<size_t>(e);
}

template<typename I>
constexpr std::enable_if_t<std::is_integral_v<I>, size_t> index(I i) noexcept { return static_cast<size_t>(i); }

template<typename E>
constexpr std::enable_if_t<std::is_enum_v<E>, E> next(E e) noexcept(noDebug) {
	static_assert(E::max > E::min); // mostly to assert E::min and E::max exist
	DEBUG_ASSERT(e < E::max);
	return E(index(e) + 1);
}

template<typename E>
constexpr std::enable_if_t<std::is_enum_v<E>, void> inc(E &e) noexcept(noDebug) {
	static_assert(E::max > E::min);
	DEBUG_ASSERT(e <= E::max);
	e = E(index(e) + 1);
}

} // namespace coretools

template<typename E>
constexpr std::enable_if_t<std::is_enum_v<E>, E &> operator++(E &e) noexcept {
	coretools::inc(e);
	return e;
}

#endif
