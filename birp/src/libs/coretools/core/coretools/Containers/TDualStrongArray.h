/*
 * TDualStrongArray.h
 *
 *  Created on: April 5, 2022
 *      Author: Andreas Füglistaler
 */

#ifndef TYPES_TDUALSTRONGARRAY_H_
#define TYPES_TDUALSTRONGARRAY_H_

#include <array>
#include <cmath>
#include <cstddef>
#include <stdexcept>
#include <type_traits>

#include "coretools/Containers/TStrongArray.h"
#include "coretools/Main/TError.h"
#include "coretools/traits.h"

namespace coretools {

enum class ABType : uint8_t { min = 0, A = min, B = 1, max = 2 };

template<typename Type, size_t Nmin, size_t Nmax>
constexpr std::array<Type, Nmax> pad(const std::array<Type, Nmin> &in) {
	std::array<Type, Nmax> out{};
	for (size_t i = 0; i < Nmin; ++i) out[i] = in[i];
	return out;
}


template<typename Type, size_t N> constexpr std::array<Type, N> makeArray(const Type *in, size_t n = N) {
	std::array<Type, N> out{};
	for (size_t i = 0; i < n; ++i) out[i] = in[i];
	return out;
}

template<typename Type, typename IndexA, typename IndexB, typename DualType=ABType, size_t NA = index(IndexA::max),
         size_t NB = index(IndexB::max)>
class TDualStrongArray {
	static_assert(NA != NB);
	static_assert(sizeof(DualType) == 1);
	static_assert(isTaggable_v<Type>);
	static_assert(std::is_integral_v<IndexA> || isScopedEnum_v<IndexA>);
	static_assert(std::is_integral_v<IndexB> || isScopedEnum_v<IndexB>);

private:
	static constexpr size_t Nmax = std::max(NA, NB);
	static constexpr size_t Nmin = std::min(NA, NB);
	static constexpr TStrongArray<size_t, DualType> _Ns{{NA, NB}};

	static constexpr DualType _shorter = NA < NB ? DualType(0) : DualType(1);
	static constexpr DualType _longer  = NA < NB ? DualType(1) : DualType(0);

	std::array<Type, Nmax> _data;

public:
	using iterator               = typename std::array<Type, Nmax>::iterator;
	using const_iterator         = typename std::array<Type, Nmax>::const_iterator;
	using reverse_iterator       = typename std::array<Type, Nmax>::reverse_iterator;
	using const_reverse_iterator = typename std::array<Type, Nmax>::const_reverse_iterator;
	using value_type             = Type;
	using size_type              = size_t;
	using dual_type              = DualType;

	constexpr DualType type() const noexcept {
		if constexpr (hasIsTaggable_v<Type>) {
			return _data.back().isTag() ? _shorter : _longer;
		} else {
			return std::isnan(_data.back()) ? _shorter : _longer;;
		}
	}
	constexpr void setType(DualType T) noexcept {
		if constexpr (hasIsTaggable_v<Type>) {
			if (T == _shorter) {
				_data.back().setAsTag();
			} else if (_data.back().isTag()) {
				// reset
				_data.back() = Type{};
			}
			// else: nothing to do
		} else {
			if (T == _shorter) {
				_data.back() = NAN;
			} else if (std::isnan(_data.back())) {
				// reset
				_data.back() = Type{};
			}
		}
	}

	constexpr TDualStrongArray() = default;
	constexpr TDualStrongArray(Type Value, DualType DType) {
		fill(Value);
		setType(DType);
	}
	constexpr TDualStrongArray(const std::array<Type, Nmax> &init) : _data(init){
		setType(_longer);
	}
	constexpr TDualStrongArray(const std::array<Type, Nmin> &init)
	    : _data(pad<Type, Nmin, Nmax>(init)) {
		setType(_shorter);
	}

	constexpr TDualStrongArray(const TStrongArray<Type, IndexA, NA> &saA)
		: _data(makeArray<Type, Nmax>(saA.data(), saA.size())) {
		setType(DualType{0});
	}
	constexpr TDualStrongArray(const TStrongArray<Type, IndexB, NB> &saB)
		: _data(makeArray<Type, Nmax>(saB.data(), saB.size())) {
		setType(DualType{1});
	}

	constexpr bool isType(DualType t) const noexcept { return t == type(); }

	template<DualType T> constexpr bool isType() const noexcept {
		return T == type();
	}

	template<typename Index>
	constexpr Type &operator[](Index i) noexcept(noDebug) {
		DEBUG_ASSERT(index(i) < size());
		if constexpr (std::is_same_v<Index, IndexA>) {
			DEBUG_ASSERT(type() == DualType{0});
		} else if constexpr (std::is_same_v<Index, IndexB>) {
			DEBUG_ASSERT(type() == DualType{1});
		} else if constexpr (std::is_integral_v<Index>){
			static_assert(std::is_integral_v<IndexA> || std::is_integral_v<IndexB>);
			DEBUG_ASSERT((type() == DualType{0} && std::is_integral_v<IndexA>) ||
						 (type() == DualType{1} && std::is_integral_v<IndexB>));
		} else {
			static_assert(std::is_same_v<Index, IndexA>); // will always fail
		}
		return _data[index(i)];
	}

	template<typename Index>
	constexpr const Type &operator[](Index i) const noexcept(noDebug) {
		DEBUG_ASSERT(index(i) < size());
		if constexpr (std::is_same_v<Index, IndexA>) {
			DEBUG_ASSERT(type() == DualType{0});
		} else if constexpr (std::is_same_v<Index, IndexB>) {
			DEBUG_ASSERT(type() == DualType{1});
		} else if constexpr (std::is_integral_v<Index>){
			static_assert(std::is_integral_v<IndexA> || std::is_integral_v<IndexB>);
			DEBUG_ASSERT((type() == DualType{0} && std::is_integral_v<IndexA>) ||
						 (type() == DualType{1} && std::is_integral_v<IndexB>));
		} else {
			static_assert(std::is_same_v<Index, IndexA>); // will always fail
		}
		return _data[index(i)];
	}

	template<typename Index> constexpr Type &at(Index i) {
		if (index(i) >= size()) throw std::out_of_range("TDualArray out of range");

		if constexpr (std::is_same_v<Index, IndexA>) {
			if (type() != DualType{0}) throw std::out_of_range("TDualStrongArray is not first type");
		} else if constexpr (std::is_same_v<Index, IndexB>) {
			if (type() != DualType{1}) throw std::out_of_range("TDualStrongArray is not second type");
		} else if constexpr (std::is_integral_v<Index>) {
			static_assert(std::is_integral_v<IndexA> || std::is_integral_v<IndexB>);
			if (type() == DualType{0} && !std::is_integral_v<IndexA>) throw std::out_of_range("TDualStrongArray is not first type");
			if (type() == DualType{1} && !std::is_integral_v<IndexB>) throw std::out_of_range("TDualStrongArray is not second type");
		} else {
			static_assert(std::is_same_v<Index, IndexA>); // will always fail
		}

		return _data.at(index(i));
	}

	template<typename Index> constexpr const Type &at(Index i) const {
		if (index(i) >= size()) throw std::out_of_range("TDualArray out of range");

		if constexpr (std::is_same_v<Index, IndexA>) {
			if (type() != DualType{0}) throw std::out_of_range("TDualStrongArray is not first type");
		} else if constexpr (std::is_same_v<Index, IndexB>) {
			if (type() != DualType{1}) throw std::out_of_range("TDualStrongArray is not second type");
		} else if constexpr (std::is_integral_v<Index>) {
			static_assert(std::is_integral_v<IndexA> || std::is_integral_v<IndexB>);
			if (type() == DualType{0} && !std::is_integral_v<IndexA>) throw std::out_of_range("TDualStrongArray is not first type");
			if (type() == DualType{1} && !std::is_integral_v<IndexB>) throw std::out_of_range("TDualStrongArray is not second type");
		} else {
			static_assert(std::is_same_v<Index, IndexA>); // will always fail
		}

		return _data.at(index(i));
	}

	constexpr Type &front() noexcept { return _data.front(); }
	constexpr const Type &front() const noexcept { return _data.front(); }

	constexpr Type &back() noexcept { return _data[_Ns[type()] - 1]; }
	constexpr const Type &back() const noexcept { return _data[_Ns[type()] - 1]; }

	constexpr Type *data() noexcept { return _data.data(); }
	constexpr const Type *data() const noexcept { return _data.data(); }

	constexpr bool empty() const noexcept { return std::min(NA, NB) == 0; }
	constexpr size_t size() const noexcept { return _Ns[type()]; }
	constexpr size_t max_size() const noexcept { return _data.max_size(); }

	constexpr void fill(const Type &value) noexcept {
		//_data.fill(value); not constexpr
		for (auto& d: _data) d = value;
	}
	void swap(TDualStrongArray<Type, IndexA, IndexB, DualType, NA, NB> &other) noexcept(std::is_nothrow_swappable_v<Type>) { _data.swap(other._data); }

	constexpr iterator begin() noexcept { return _data.begin(); }
	constexpr iterator end() noexcept { return _data.begin() + _Ns[type()]; }

	constexpr const_iterator begin() const noexcept { return _data.begin(); }
	constexpr const_iterator end() const noexcept { return _data.begin() + _Ns[type()]; }

	constexpr const_iterator cbegin() const noexcept { return begin(); }
	constexpr const_iterator cend() const noexcept { return end(); }

	constexpr reverse_iterator rbegin() noexcept { return _data.rend() - _Ns[type()]; }
	constexpr reverse_iterator rend() noexcept { return _data.rend(); };

	constexpr const_reverse_iterator rbegin() const noexcept { return _data.rend() - _Ns[type()]; }
	constexpr const_reverse_iterator rend() const noexcept { return _data.rend(); }

	constexpr const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	constexpr const_reverse_iterator crend() const noexcept { return rend(); }
};

template<typename T, size_t NA, size_t NB>
using TDualArray = TDualStrongArray<T, size_t, size_t, ABType, NA, NB>;
} // namespace coretools

#endif
