#ifndef TYPES_TSTRONGARRAY_H_
#define TYPES_TSTRONGARRAY_H_

#include <array>
#include <stddef.h>
#include <type_traits>

#include "coretools/enum.h"


namespace coretools {

template<class T, class = void>
struct isArithmetic : std::false_type {};

template<class T>
struct isArithmetic<T, std::void_t<
				  decltype(std::declval<T>() + std::declval<T>()),
                  decltype(std::declval<T>() - std::declval<T>()),
                  decltype(std::declval<T>() * std::declval<T>()),
                  decltype(std::declval<T>() / std::declval<T>())>> 
       : std::true_type {};

template<class T>
constexpr bool isArithmetic_v = isArithmetic<T>::value;

template<typename Type, size_t N> constexpr std::array<Type, N> fill_array(Type v) {
	std::array<Type, N> ret{};
	for (auto &d : ret) d = v; // std::array::fill is not constexpr
	return ret;
}

template<typename Type, typename Index, size_t N = index(Index::max)> class TStrongArray {
private:
	std::array<Type, N> _data;

public:
	using iterator               = typename std::array<Type, N>::iterator;
	using const_iterator         = typename std::array<Type, N>::const_iterator;
	using reverse_iterator       = typename std::array<Type, N>::reverse_iterator;
	using const_reverse_iterator = typename std::array<Type, N>::const_reverse_iterator;
	using value_type             = Type;
	using index_type             = Index;
	using size_type              = size_t;

	static constexpr size_t capacity = N;
	static_assert(capacity > 0);

	constexpr TStrongArray() = default;
	constexpr explicit TStrongArray(Type v) noexcept : _data(fill_array<Type, N>(v)) {}
	constexpr TStrongArray(std::array<Type, N> init) : _data(std::move(init)) {}

	constexpr Type &operator[](Index i) noexcept {
		assert(index(i) < N);
		return _data[index(i)];
	}
	constexpr const Type &operator[](Index i) const noexcept {
		assert(index(i) < N);
		return _data[index(i)];
	}

	constexpr Type &at(Index i) { return _data.at(index(i)); }
	constexpr const Type &at(Index i) const { return _data.at(index(i)); }

	constexpr Type &front() noexcept { return _data.front(); }
	constexpr const Type &front() const noexcept { return _data.front(); }

	constexpr Type &back() noexcept { return _data.back(); }
	constexpr const Type &back() const noexcept { return _data.back(); }

	constexpr Type *data() noexcept { return _data.data(); }
	constexpr const Type *data() const noexcept { return _data.data(); }

	constexpr size_t size() const noexcept { return _data.size(); }
	constexpr size_t empty() const noexcept { return false; }
	constexpr size_t max_size() const noexcept { return _data.max_size(); }

	void fill(const Type &value) noexcept { _data.fill(value); }
	void swap(TStrongArray<Type, Index, N> &other) noexcept(std::is_nothrow_swappable_v<Type>) {
		_data.swap(other._data);
	}

	constexpr iterator begin() noexcept { return _data.begin(); }
	constexpr iterator end() noexcept { return _data.end(); }

	constexpr const_iterator begin() const noexcept { return _data.begin(); }
	constexpr const_iterator end() const noexcept { return _data.end(); }

	constexpr const_iterator cbegin() const noexcept { return begin(); }
	constexpr const_iterator cend() const noexcept { return end(); }

	constexpr reverse_iterator rbegin() noexcept { return _data.rbegin(); }
	constexpr reverse_iterator rend() noexcept { return _data.rend(); }

	constexpr const_reverse_iterator rbegin() const noexcept { return _data.rbegin(); }
	constexpr const_reverse_iterator rend() const noexcept { return _data.rend(); }

	constexpr const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	constexpr const_reverse_iterator crend() const noexcept { return rend(); }

	constexpr TStrongArray<Type, Index, N> &operator+=(const TStrongArray<Type, Index, N> &rhs) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (size_t i = 0; i < N; ++i) {
			_data[i] += rhs._data[i];
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator+=(const Type &val) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (auto& d: _data) {
			d += val;
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator-=(const TStrongArray<Type, Index, N> &rhs) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (size_t i = 0; i < N; ++i) {
			_data[i] -= rhs._data[i];
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator-=(const Type &val) noexcept {
		static_assert(isArithmetic_v<Type>);
		static_assert(isArithmetic_v<Type>);
		for (auto& d: _data) {
			d -= val;
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator*=(const TStrongArray<Type, Index, N> &rhs) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (size_t i = 0; i < N; ++i) {
			_data[i] *= rhs._data[i];
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator*=(const Type &val) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (auto& d: _data) {
			d *= val;
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator/=(const TStrongArray<Type, Index, N> &rhs) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (size_t i = 0; i < N; ++i) {
			_data[i] /= rhs._data[i];
		}
		return *this;
	}

	constexpr TStrongArray<Type, Index, N> &operator/=(const Type &val) noexcept {
		static_assert(isArithmetic_v<Type>);
		for (auto& d: _data) {
			d /= val;
		}
		return *this;
	}

	friend bool operator==(const TStrongArray<Type, Index, N>& lhs, const TStrongArray<Type, Index, N>& rhs) noexcept {
		return lhs._data == rhs._data;
	}

	friend bool operator!=(const TStrongArray<Type, Index, N>& lhs, const TStrongArray<Type, Index, N>& rhs) noexcept {
		return lhs._data != rhs._data;
	}

	friend bool operator<(const TStrongArray<Type, Index, N>& lhs, const TStrongArray<Type, Index, N>& rhs) noexcept {
		return lhs._data < rhs._data;
	}

	friend bool operator<=(const TStrongArray<Type, Index, N>& lhs, const TStrongArray<Type, Index, N>& rhs) noexcept {
		return lhs._data <= rhs._data;
	}

	friend bool operator>(const TStrongArray<Type, Index, N>& lhs, const TStrongArray<Type, Index, N>& rhs) noexcept {
		return lhs._data > rhs._data;
	}

	friend bool operator>=(const TStrongArray<Type, Index, N>& lhs, const TStrongArray<Type, Index, N>& rhs) noexcept {
		return lhs._data >= rhs._data;
	}
};

template<typename Type, size_t N>
using TArray = TStrongArray<Type, size_t, N>;

// +
template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator+(TStrongArray<Type, Index, N> lhs,
												 const TStrongArray<Type, Index, N> &rhs) {
	static_assert(isArithmetic_v<Type>);
	return lhs += rhs;
}

template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator+(TStrongArray<Type, Index, N> lhs, const Type &val) {
	static_assert(isArithmetic_v<Type>);
	return lhs += val;
}

// -
template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator-(TStrongArray<Type, Index, N> lhs,
												 const TStrongArray<Type, Index, N> &rhs) {
	static_assert(isArithmetic_v<Type>);
	return lhs -= rhs;
}

template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator-(TStrongArray<Type, Index, N> lhs, const Type &val) {
	return lhs -= val;
}

template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator-(TStrongArray<Type, Index, N> lhs) {
	static_assert(isArithmetic_v<Type>);
	for (auto& v: lhs) v = -v;
	return lhs;
}

// *
template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator*(TStrongArray<Type, Index, N> lhs,
												 const TStrongArray<Type, Index, N> &rhs) {
	static_assert(isArithmetic_v<Type>);
	return lhs *= rhs;
}

template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator*(const Type &val, TStrongArray<Type, Index, N> rhs) {
	static_assert(isArithmetic_v<Type>);
	return rhs *= val;
}

// /
template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator/(TStrongArray<Type, Index, N> lhs,
												 const TStrongArray<Type, Index, N> &rhs) {
	static_assert(isArithmetic_v<Type>);
	return lhs /= rhs;
}

template<typename Type, typename Index, size_t N>
constexpr TStrongArray<Type, Index, N> operator/(TStrongArray<Type, Index, N> lhs, const Type &val) {
	static_assert(isArithmetic_v<Type>);
	return lhs /= val;
}

} // namespace coretools

#endif
