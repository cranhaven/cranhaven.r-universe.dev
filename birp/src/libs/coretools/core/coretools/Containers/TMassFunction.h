/*
 * TMassFunction.h
 *
 *  Created on: Apr 19, 2022
 *      Author: andreas
 */

#ifndef TMASSFUNCTION_H_
#define TMASSFUNCTION_H_

#include <algorithm>
#include <functional>
#include <type_traits>

#include "coretools/Containers/CTFraction.h"
#include "coretools/Containers/TStrongArray.h"
#include "coretools/traits.h"

namespace coretools {

template<typename Type, typename Index, size_t N = index(Index::max), typename Sum=CT1<Type>>
class TStrongMassFunction {
private:
	using StrongArray = TStrongArray<Type, Index, N>;
	using Underlying = underlyingType_t<Type>;

	StrongArray _data{Type(Sum::value/N)};

	template<typename T>
	static constexpr Underlying _sum(const T* data) {
		static_assert(std::is_same_v<T, Type> || std::is_same_v<T, Underlying>);
		auto s = Underlying{};
		for (size_t i = 0; i < N; ++i) {
			s += data[i];
		}
		return s;
	}

	// Naming convention:
	// normalize: return a new, normalized constainer
	// scale: in-place (mutate) scale the given container 
	constexpr void _scale(Underlying sum) {
		for (auto &d : _data) d = Type(d / sum);
	}

	constexpr void _scale() {
		_scale(_sum(_data.data())*Sum::value);
	}

	template<typename T>
	constexpr TStrongMassFunction(const T* data) {
		static_assert(std::is_same_v<T, Type> || std::is_same_v<T, Underlying>);
		const Underlying sum = _sum(data) * Sum::value;
		for (size_t i = 0; i < N; ++i) {
			_data.data()[i] = Type(data[i]/sum);
		}
	}
	    

public:
	using iterator               = typename StrongArray::iterator;
	using const_iterator         = typename StrongArray::const_iterator;
	using reverse_iterator       = typename StrongArray::reverse_iterator;
	using const_reverse_iterator = typename StrongArray::const_reverse_iterator;
	using value_type             = Type;
	using index_type             = Index;
	using size_type              = size_t;

	static constexpr size_t capacity = N;
	static constexpr Type S          = Sum::value;

	constexpr TStrongMassFunction() = default;

	static constexpr TStrongMassFunction normalize(const std::array<Type, N> &init) {
		return TStrongMassFunction(init.data());
	}

	template<typename U = Underlying, std::enable_if_t<!std::is_same_v<Type, U>, bool> = true>
	static constexpr TStrongMassFunction normalize(const std::array<U, N> &init) {
		return TStrongMassFunction(init.data());
	}

	static constexpr TStrongMassFunction normalize(const TStrongArray<Type, Index, N> &init) {
		return TStrongMassFunction(init.data());
	}
	template<typename U = Underlying, std::enable_if_t<!std::is_same_v<Type, U>, bool> = true>
	static constexpr TStrongMassFunction normalize(const TStrongArray<U, Index, N> &init) {
		return TStrongMassFunction(init.data());
	}

	template<typename Container1, typename Container2, typename BinaryOperation>
	static constexpr TStrongMassFunction normalize(const Container1 &x, const Container2 &y, BinaryOperation op) {
		assert(x.size() == N);
		assert(y.size() == N);
		TStrongMassFunction mf;
		std::transform(x.cbegin(), x.cend(), y.cbegin(), mf._data.begin(), op);
		mf._scale();
		return mf;
	}

	constexpr explicit operator StrongArray() const noexcept {return _data;}

	void for_each(std::function<Type(Type)> fn) noexcept {
		Underlying s{};
		for (auto& d: _data) {
			d  = fn(d);
			s += d;
		}
		_scale(s*Sum::value);
	}

	void for_each_index(std::function<Type(Type, Index)> fn) noexcept {
		Underlying s{};
		for (Index i = Index{0}; i < Index{N}; ++i) {
			auto& d = _data[i];
			d  = fn(d, i);
			s += d;
		}
		_scale(s*Sum::value);
	}

	constexpr const Type &operator[](Index i) const noexcept { return _data[i]; }
	constexpr const Type &at(Index i) const { return _data.at(i); }
	constexpr const Type &front() const noexcept { return _data.front(); }
	constexpr const Type &back() const noexcept { return _data.back(); }
	constexpr const Type *data() const noexcept { return _data.data(); }

	constexpr bool empty() const noexcept { return _data.empty(); }
	constexpr size_t size() const noexcept { return _data.size(); }
	constexpr size_t max_size() const noexcept { return _data.max_size(); }

	constexpr const_iterator begin() const noexcept { return _data.begin(); }
	constexpr const_iterator end() const noexcept { return _data.end(); }

	constexpr const_iterator cbegin() const noexcept { return begin(); }
	constexpr const_iterator cend() const noexcept { return end(); }

	constexpr const_reverse_iterator rbegin() const noexcept { return _data.rbegin(); }
	constexpr const_reverse_iterator rend() const noexcept { return _data.rend(); }

	constexpr const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	constexpr const_reverse_iterator crend() const noexcept { return rend(); }
};

template<typename Type, size_t N, typename Sum=CT1<Type>>
using TMassFunction = TStrongMassFunction<Type, size_t, N, Sum>;

} // namespace coretools

#endif
