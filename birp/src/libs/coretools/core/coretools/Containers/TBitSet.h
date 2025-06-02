/*
 * TBitSet.h
 *
 *  Created on: 8.11.2021
 *      Author: Andreas
 */

#ifndef TSTRONGBITSET_H_
#define TSTRONGBITSET_H_

#include <cassert>

#include "coretools/Containers/TStrongArray.h"
#include "coretools/traits.h"

namespace coretools {

template<typename Index, size_t N = index(Index::max)> class TStrongBitSet {
public:
	using uintN_t = SmallestInteger_t<N>;

private:
	static constexpr TStrongArray<SmallestInteger_t<N>, Index, N> _getBitmasks() {
		TStrongArray<SmallestInteger_t<N>, Index, N> a{};
		for (size_t i = 0; i < N; ++i) { a.data()[i] = 1 << i; }
		return a;
	}

	uintN_t _data                                                  = 0;
	static constexpr TStrongArray<SmallestInteger_t<N>, Index, N> _bitmasks = _getBitmasks();

public:
	constexpr TStrongBitSet() = default;
	constexpr explicit TStrongBitSet(uintN_t in) noexcept : _data(in) {
		if constexpr (N < 8 * sizeof(uintN_t)) assert(in < (1u << N));
	}

	constexpr void reset() noexcept { _data = 0; }
	constexpr uintN_t to_ulong() const noexcept { return _data; }

	template<Index i> constexpr bool get() const noexcept {
		static_assert(index(i) < N);
		constexpr auto bitmask = _bitmasks[i];
		return (_data & bitmask) != 0;
	}

	template<Index i> constexpr void set() noexcept {
		static_assert(index(i) < N);
		constexpr auto bitmask = _bitmasks[i];
		_data |= bitmask;
	}

	template<Index i> constexpr void set(bool b) noexcept {
		static_assert(index(i) < N);
		constexpr auto bitmask  = _bitmasks[i];
		if (b) {
			_data |= bitmask;
		}
		else {
			constexpr auto nbitmask = ~bitmask;
			_data &= nbitmask;
		}
	}

	constexpr bool operator[](Index i) const noexcept {
		assert(index(i) < N);
		return (_data & _bitmasks[i]) != 0;
	}

	constexpr auto operator[](Index i) noexcept {
		class Voldemort { // This type cannot be named
		private:
			uintN_t &_data;
			const uintN_t _bitmask;

		public:
			constexpr Voldemort(uintN_t &data, uintN_t bitmask) noexcept : _data(data), _bitmask(bitmask) {}

			constexpr Voldemort &operator=(bool b) noexcept {
				if (b)
					_data |= _bitmask;
				else
					_data &= ~_bitmask;
				return *this;
			}
			constexpr operator bool() const noexcept { return (_data & _bitmask) != 0; }
			constexpr bool operator~() const noexcept { return (_data & _bitmask) == 0; }
		};
		assert(index(i) < N);
		return Voldemort(_data, _bitmasks[i]);
	}

	friend bool operator==(const TStrongBitSet<Index, N> &lhs, const TStrongBitSet<Index, N> &rhs) { return lhs._data == rhs._data; }
};

template<size_t N> using TBitSet = TStrongBitSet<size_t, N>;

}; // namespace coretools

#endif
