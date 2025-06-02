/*
 * TDualArray.h
 *
 *  Created on: April 4, 2022
 *      Author: Andreas Füglistaler
 */

#ifndef TYPES_TDUALARRAY_H_
#define TYPES_TDUALARRAY_H_

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <stdexcept>

#include "coretools/Containers/TStrongArray.h"

namespace coretools {

enum class ABType : uint8_t { min = 0, A = min, B = 1, max = 2 };

template<typename Type, size_t Nmin, size_t Nmax>
constexpr std::array<Type, Nmax> pad(const std::array<Type, Nmin> &in) {
	std::array<Type, Nmax> out{};
	for (size_t i = 0; i < Nmin; ++i) out[i] = in[i];
	return out;
}

template<typename Type, size_t NA, size_t NB, typename DualType=ABType> class TDualArray {
	static_assert(sizeof(DualType) == 1);
private:
	static constexpr size_t Nmax = std::max(NA, NB);
	static constexpr size_t Nmin = std::min(NA, NB);
	static constexpr TStrongArray<size_t, DualType> _Ns{std::array{NA, NB}};

	std::array<Type, Nmax> _data;
public:
	using iterator               = typename std::array<Type, Nmax>::iterator;
	using const_iterator         = typename std::array<Type, Nmax>::const_iterator;
	using reverse_iterator       = typename std::array<Type, Nmax>::reverse_iterator;
	using const_reverse_iterator = typename std::array<Type, Nmax>::const_reverse_iterator;
	using value_type             = Type;
	using size_type              = size_t;

	DualType type;

	constexpr TDualArray() = default;
	constexpr TDualArray(const std::array<Type, Nmax> &init) : _data({init}), type(DualType(NB == Nmax)) {}
	constexpr TDualArray(const std::array<Type, Nmin> &init)
	    : _data(pad<Type, Nmin, Nmax>(init)), type(DualType(NB == Nmin)) {}

	constexpr bool isType(DualType t) const noexcept { return t == type; }

	template<DualType T> constexpr bool isType() const noexcept {
		if constexpr (bool(T))
			return bool(type);
		else
			return !bool(type);
	}

	constexpr Type &operator[](size_t i) noexcept {
		assert(i < _Ns[type]);
		return _data[i];
	}
	constexpr const Type &operator[](size_t i) const noexcept {
		assert(i < _Ns[type]);
		return _data[i];
	}

	constexpr Type &at(size_t i) {
		if (i >= _Ns[type]) throw std::out_of_range("TDualArray out of range");
		return _data.at(i);
	}

	constexpr const Type &at(size_t i) const {
		if (i >= _Ns[type]) throw std::out_of_range("TDualArray out of range");
		return _data.at(i);
	}

	constexpr Type &front() noexcept { return _data.front(); }
	constexpr const Type &front() const noexcept { return _data.front(); }

	constexpr Type &back() noexcept { return _data[_Ns[type] - 1]; }
	constexpr const Type &back() const noexcept { return _data[_Ns[type] - 1]; }

	constexpr Type *data() noexcept { return _data.data(); }
	constexpr const Type *data() const noexcept { return _data.data(); }

	constexpr bool empty() const noexcept { return std::min(NA, NB) == 0; }
	constexpr size_t size() const noexcept { return _Ns[type]; }
	constexpr size_t max_size() const noexcept { return _data.size(); }

	void fill(const Type &value) noexcept { _data.fill(value); }
	void swap(TDualArray<Type, NA, NB, DualType> &other) noexcept(std::is_nothrow_swappable_v<Type>) { _data.swap(other._data); }

	constexpr iterator begin() noexcept { return _data.begin(); }
	constexpr iterator end() noexcept { return _data.begin() + _Ns[type]; }

	constexpr const_iterator begin() const noexcept { return _data.begin(); }
	constexpr const_iterator end() const noexcept { return _data.begin() + _Ns[type]; }

	constexpr const_iterator cbegin() const noexcept { return begin(); }
	constexpr const_iterator cend() const noexcept { return end(); }

	constexpr reverse_iterator rbegin() noexcept { return _data.rend() - _Ns[type]; }
	constexpr reverse_iterator rend() noexcept { return _data.rend(); };

	constexpr const_reverse_iterator rbegin() const noexcept { return _data.rend() - _Ns[type]; }
	constexpr const_reverse_iterator rend() const noexcept { return _data.rend(); }

	constexpr const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	constexpr const_reverse_iterator crend() const noexcept { return rend(); }
};
} // namespace coretools

#endif
