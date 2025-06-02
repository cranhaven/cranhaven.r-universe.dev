/*
 * TDualStrongArray.h
 *
 *  Created on: April 5, 2022
 *      Author: Andreas Füglistaler
 */

#ifndef TYPES_TDUALSTRONGARRAY_H_
#define TYPES_TDUALSTRONGARRAY_H_

#include <array>
#include <cassert>
#include <cstddef>
#include <stdexcept>

#include "coretools/Containers/TDualArray.h"
#include "coretools/Containers/TStrongArray.h"

namespace coretools {

template<typename Type, size_t N> constexpr std::array<Type, N> makeArray(const Type *in, size_t n = N) {
	std::array<Type, N> out{};
	for (size_t i = 0; i < n; ++i) out[i] = in[i];
	return out;
}

template<typename Type, typename IndexA, typename IndexB, size_t NA = index(IndexA::max),
         size_t NB = index(IndexB::max), typename DualType=ABType>
class TDualStrongArray {
	static_assert(NA != NB);
	static_assert(sizeof(DualType) == 1);
private:
	static constexpr size_t Nmax = std::max(NA, NB);
	static constexpr size_t Nmin = std::min(NA, NB);
	static constexpr TStrongArray<size_t, DualType> _Ns{{NA, NB}};

	std::array<Type, Nmax> _data;
public:
	using iterator               = typename std::array<Type, Nmax>::iterator;
	using const_iterator         = typename std::array<Type, Nmax>::const_iterator;
	using reverse_iterator       = typename std::array<Type, Nmax>::reverse_iterator;
	using const_reverse_iterator = typename std::array<Type, Nmax>::const_reverse_iterator;
	using value_type             = Type;
	using size_type              = size_t;
	using dual_type              = DualType;

	DualType type;

	constexpr TDualStrongArray() = default;
	constexpr TDualStrongArray(Type Value, DualType DType) : type(DType) {
		fill(Value);
	}
	constexpr TDualStrongArray(const std::array<Type, Nmax> &init) : _data(init), type(DualType(NB == Nmax)) {}
	constexpr TDualStrongArray(const std::array<Type, Nmin> &init)
	    : _data(pad<Type, Nmin, Nmax>(init)), type(DualType(NB == Nmax)) {}

	constexpr TDualStrongArray(const TStrongArray<Type, IndexA, NA> &saA)
		: _data(makeArray<Type, Nmax>(saA.data(), saA.size())), type(DualType{0}) {}
	constexpr TDualStrongArray(const TStrongArray<Type, IndexB, NB> &saB)
		: _data(makeArray<Type, Nmax>(saB.data(), saB.size())), type(DualType{1}) {}

	constexpr bool isType(DualType t) const noexcept { return t == type; }

	template<DualType T> constexpr bool isType() const noexcept {
		if constexpr (bool(T))
			return bool(type);
		else
			return !bool(type);
	}

	constexpr Type &operator[](IndexA i) noexcept {
		assert(type == DualType{0} && index(i) < _Ns[DualType{0}]);
		return _data[index(i)];
	}
	constexpr const Type &operator[](IndexA i) const noexcept {
		assert(type == DualType{0} && index(i) < _Ns[DualType{0}]);
		return _data[index(i)];
	}

	constexpr Type &operator[](IndexB i) noexcept {
		assert(type == DualType{1} && index(i) < _Ns[DualType{1}]);
		return _data[index(i)];
	}
	constexpr const Type &operator[](IndexB i) const noexcept {
		assert(type == DualType{1} && index(i) < _Ns[DualType{1}]);
		return _data[index(i)];
	}

	constexpr Type &at(IndexA i) {
		if (type != DualType{0}) throw std::out_of_range("TDualStrongArray is not first type");
		if (index(i) >= _Ns[DualType{0}]) throw std::out_of_range("TDualArray out of range");
		return _data.at(index(i));
	}
	constexpr const Type &at(IndexA i) const {
		if (type != DualType{0}) throw std::out_of_range("TDualStrongArray is not first type");
		if (index(i) >= _Ns[DualType{0}]) throw std::out_of_range("TDualArray out of range");
		return _data.at(index(i));
	}

	constexpr Type &at(IndexB i) {
		if (type != DualType{1}) throw std::out_of_range("TDualStrongArray is not second type");
		if (index(i) >= _Ns[DualType{1}]) throw std::out_of_range("TDualArray out of range");
		return _data.at(index(i));
	}
	constexpr const Type &at(IndexB i) const {
		if (type != DualType{1}) throw std::out_of_range("TDualStrongArray is not second type");
		if (index(i) >= _Ns[DualType{1}]) throw std::out_of_range("TDualArray out of range");
		return _data.at(index(i));
	}

	constexpr Type &front() noexcept { return _data.front(); }
	constexpr const Type &front() const noexcept { return _data.front(); }

	constexpr Type &back() noexcept { return _data[_Ns[type] - 1]; }
	constexpr const Type &back() const noexcept { return _data[_Ns[type] - 1]; }

	constexpr Type *data() noexcept { return _data.data(); }
	constexpr const Type *data() const noexcept { return _data.data(); }

	constexpr bool empty() const noexcept { return std::min(NA, NB) == 0; }
	constexpr size_t size() const noexcept { return _Ns[type]; }
	constexpr size_t max_size() const noexcept { return _data.max_size(); }

	constexpr void fill(const Type &value) noexcept {
		//_data.fill(value); not constexpr
		for (auto& d: _data) d = value;
	}
	void swap(TDualStrongArray<Type, IndexA, IndexB, NA, NB, DualType> &other) noexcept(std::is_nothrow_swappable_v<Type>) { _data.swap(other._data); }

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
