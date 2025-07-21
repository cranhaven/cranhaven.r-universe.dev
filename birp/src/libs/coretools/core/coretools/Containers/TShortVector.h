/*
 * TShortVector.h
 *
 *  Created on: March 21, 2022
 *      Author: Andreas Füglistaler
 */

#ifndef TYPES_TSHORTVECTOR_H_
#define TYPES_TSHORTVECTOR_H_

#include <cstddef>
#include <iterator>
#include <stdexcept>

#include "coretools/Main/TError.h"

namespace coretools {

template<typename Type, size_t N_max> class TShortVector {
private:
	alignas(Type) std::byte _data[N_max * sizeof(Type)];
	size_t _size = 0;

	void _pop_n(size_t n) {
		DEBUG_ASSERT(size() >= n);
		for (size_t _ = 0; _ < n; ++_) { pop_back(); }
	}

	void _push_n(size_t n, const Type& v) {
		DEBUG_ASSERT(size() + n <= N_max);
		for (size_t _ = 0; _ < n; ++_) { push_back(v); }
	}

	constexpr bool _hasCapacity() const noexcept { return size() < N_max; }

public:
	using value_type    = Type;
	using size_type     = size_t;
	using pointer       = Type *;
	using const_pointer = const Type *;

	using iterator               = pointer;
	using const_iterator         = const_pointer;
	using reverse_iterator       = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	TShortVector() noexcept = default;

	explicit TShortVector(size_t N, const Type& value = {}) {
		_push_n(N, value);
	}

	TShortVector(std::initializer_list<Type> init) {
		DEBUG_ASSERT(init.size() <= N_max);
		std::copy(init.begin(), init.end(), std::back_inserter(*this));
	}

	template<typename It>
	TShortVector(It first, It end) {
		std::copy(first, end, std::back_inserter(*this));
	}

	template<size_t N>
	TShortVector(const TShortVector<Type, N>& other) {
		DEBUG_ASSERT(other.size() <= N_max);
		std::copy(other.begin(), other.end(), std::back_inserter(*this));
	}

	template<size_t N>
	TShortVector& operator=(const TShortVector<Type, N>& other) {
		DEBUG_ASSERT(other.size() <= N_max);
		if (size() < other.size()) {
			std::copy(other.begin(), other.begin() + size(), begin());
			std::copy(other.begin() + size(), other.end(), std::back_inserter(*this));
		} else {
			while (size() > other.size()) {
				pop_back();
			}
			std::copy(other.begin(), other.end(), begin());
		}
		return *this;
	}

	// move operations make no sence as capacity has to be copied
	TShortVector(TShortVector&&) = delete;
	TShortVector& operator=(TShortVector&&) = delete;

	~TShortVector() { clear(); }

	void push_back(const Type &value) {
		DEBUG_ASSERT(_hasCapacity());
		::new (data() + size()) Type(value);
		++_size;
	}

	void push_back(Type &&value) {
		DEBUG_ASSERT(_hasCapacity());
		::new (data() + size()) Type(std::move(value));
		++_size;
	}

	void pop_back() {
		DEBUG_ASSERT(!empty());
		std::destroy_at(&back());
		--_size;
	}

	void clear() { _pop_n(size()); }

	void resize(size_t N, const Type &value = {}) {
		DEBUG_ASSERT(N <= N_max);

		if (N < size()) {
			_pop_n(size() - N);
		} else {
			_push_n(N - size(), value);
		}
	}

	void assign(size_t N, const Type &value) {
		DEBUG_ASSERT(N <= N_max);

		if (N < size()) {
			_pop_n(size() - N);
			std::fill(begin(), end(), value);
		} else {
			std::fill(begin(), end(), value);
			std::fill_n(std::back_inserter(*this), N - size(), value);
		}
	}

	const Type &operator[](size_t i) const noexcept(noDebug) {
		DEBUG_ASSERT(i < size());
		return *(data() + i);
	}

	Type &operator[](size_t i) noexcept(noDebug) {
		DEBUG_ASSERT(i < size());
		return *(data() + i);
	}

	Type &at(size_t i) {
		if (i >= size()) throw std::out_of_range("Array out of range");
		return operator[](i);
	}
	const Type &at(size_t i) const {
		if (i >= size()) throw std::out_of_range("Array out of range");
		return operator[](i);
	}

	Type &front() noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *data();
	}
	const Type &front() const noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *data();
	}

	Type &back() noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *(data() + size() - 1);
	}
	const Type &back() const noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *(data() + size() - 1);
	}

	pointer data() noexcept { return reinterpret_cast<Type *>(std::addressof(_data)); }
	const_pointer data() const noexcept { return reinterpret_cast<Type const *>(std::addressof(_data)); }

	constexpr bool empty() const noexcept { return !size(); }

	constexpr size_t size() const noexcept { return _size; }

	constexpr size_t max_size() const noexcept { return N_max; }

	iterator begin() noexcept { return data(); }
	iterator end() noexcept { return begin() + size(); }

	const_iterator begin() const noexcept { return data(); }
	const_iterator end() const noexcept { return begin() + size(); }

	const_iterator cbegin() const noexcept { return begin(); }
	const_iterator cend() noexcept { return end(); }

	reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }
	reverse_iterator rend() noexcept { return rbegin() + size(); }

	const_reverse_iterator rbegin() const noexcept { return const_reverse_iterator(end()); }
	const_reverse_iterator rend() const noexcept { return rbegin() + size(); }

	const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	const_reverse_iterator crend() const noexcept { return rend(); }
};
} // namespace coretools

#endif
