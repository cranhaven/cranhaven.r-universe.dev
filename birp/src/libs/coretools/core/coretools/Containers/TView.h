/*
 * TView.h
 *
 *  Created on: 4.11.2022
 *      Author: Andreas
 */

#ifndef TVIEW_H_
#define TVIEW_H_

#include "coretools/Main/TError.h"
#include "coretools/Types/TSomeProbability.h"
#include <cstddef>
#include <iterator>
#include <type_traits>
#include <algorithm>
#include <initializer_list>
#include <stdexcept>

namespace coretools {

template<typename Type> class TView {
	Type *_begin = nullptr;
	size_t _size = 0;

public:
	using value_type             = Type;
	using size_type              = size_t;
	using pointer                = Type *;
	using const_pointer          = const Type *;
	using index_type             = size_t;

	using iterator               = Type *;
	using const_iterator         = const_pointer;
	using reverse_iterator       = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	static constexpr size_type npos = size_type(-1);

	TView() = default;

	template<typename Container, std::enable_if_t<std::is_same_v<std::remove_cv_t<typename Container::value_type>, std::remove_cv_t<Type>>, bool> = true>
	TView(Container &cs) : _begin(cs.data()), _size(cs.size()) {}

	template<typename Container, std::enable_if_t<std::is_same_v<std::remove_cv_t<typename Container::value_type>, std::remove_cv_t<Type>>, bool> = true>
	TView(Container &&cs) : _begin(cs.data()), _size(cs.size()) {}

	TView(Type *data, size_t count) : _begin(data), _size(count) {}

	TView(Type *data, Type *end) : _begin(data), _size(std::distance(data, end)) {}

	/*
	  Warning, this is not ok!
	  TView<double> v{1.,2.,3.};
	  v.front(); <- dangling pointer!
	 */
	TView(std::initializer_list<Type> init) : _begin(init.begin()), _size(init.size()) {}

	template<size_t N>
	TView(Type (&arr)[N]) : _begin(arr), _size(N) {}

	size_t size() const noexcept { return _size; };
	size_t empty() const noexcept { return !_size; };

	Type &front() noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *_begin;
	}
	const Type &front() const noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *_begin;
	}

	Type &back() noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *(end() - 1);
	}
	const Type &back() const noexcept(noDebug) {
		DEBUG_ASSERT(!empty());
		return *(end() - 1);
	}

	pointer data() noexcept {
		return _begin;
	}
	const_pointer data() const noexcept {
		return _begin;
	}

	Type &operator[](size_t i) noexcept(noDebug) {
		DEBUG_ASSERT(i < size());
		return _begin[i];
	}

	const Type &operator[](size_t i) const noexcept(noDebug) {
		DEBUG_ASSERT(i < size());
		return _begin[i];
	}

	Type &at(size_t i) {
		if (i >= size()) throw std::out_of_range("TView out of range");
		return _begin[i];
	}

	const Type &at(size_t i) const noexcept {
		if (i >= size()) throw std::out_of_range("TView out of range");
		return _begin[i];
	}

	iterator begin() noexcept { return _begin; }
	iterator end() noexcept { return begin() + size(); }

	const_iterator begin() const noexcept { return _begin; }
	const_iterator end() const noexcept { return begin() + size(); }

	const_iterator cbegin() const noexcept { return begin(); }
	const_iterator cend() const noexcept { return end(); }

	reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }
	reverse_iterator rend() noexcept { return rbegin() + size(); }

	const_reverse_iterator rbegin() const noexcept { return const_reverse_iterator(end()); }
	const_reverse_iterator rend() const noexcept { return rbegin() + size(); }
	const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	const_reverse_iterator crend() const noexcept { return rend(); }

	TView subview(size_t pos = 0, size_t count = npos) const noexcept {
		return TView(_begin + pos, std::min(count, size() - pos));
	}

	void remove_prefix(size_t n) noexcept(noDebug) {
		DEBUG_ASSERT(n <= size());
		_begin += n;
		_size  -= n;
	}

	void remove_suffix(size_t n) noexcept(noDebug) {
		DEBUG_ASSERT(n <= size());
		_size -= n;
	}

	friend bool operator==(const TView<Type>& lhs, const TView<Type>& rhs) noexcept {
		return lhs._begin == rhs._begin && lhs._size == rhs._size;
	}

	friend bool operator!=(const TView<Type>& lhs, const TView<Type>& rhs) noexcept {
		return !operator==(lhs, rhs);
	}
};

template<typename Type>
using TConstView = TView<const Type>;

// Used for iterator operator->
template<typename View>
class TViewPointer {
	View _view;
public:
	TViewPointer(View Data) : _view(Data) {}
	View* operator->() {
		return &_view;
	}
	const View* operator->() const {
		return &_view;
	}
};
} // namespace coretools

#endif
