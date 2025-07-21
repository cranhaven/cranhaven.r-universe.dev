#ifndef CORETOOLS_CONTAINERS_TNESTEDITERATOR_H
#define CORETOOLS_CONTAINERS_TNESTEDITERATOR_H
#include <iterator>

#include "coretools/Containers/TView.h"
#include "coretools/Main/TError.h"

namespace coretools {

template<typename NestedContainer, bool Reversed = false, size_t I = size_t(-1)>
class TNestedIterator {
	NestedContainer *_container;
	size_t _index = 0;

public:
	using iterator_category = std::random_access_iterator_tag;
	using difference_type   = std::ptrdiff_t;

	constexpr TNestedIterator(NestedContainer *Container) : _container(Container) {}

	constexpr auto operator*() {
		if constexpr (Reversed) {
			if constexpr (I == size_t(-1)) {
				return _container->get(_container->size() - _index - 1);
			} else {
				return _container->template get<I>(_container->size() - _index - 1);
			}
		} else {
			if constexpr (I == size_t(-1)) {
				return _container->get(_index);
			} else {
				return _container->template get<I>(_index);
			}
		}
	}

	constexpr auto operator*() const {
		if constexpr (Reversed) {
			if constexpr (I == size_t(-1)) {
				return _container->get(_container->size() - _index - 1);
			} else {
				return _container->template get<I>(_container->size() - _index - 1);
			}
		} else {
			if constexpr (I == -1) {
				return _container->get(_index);
			} else {
				return _container->template get<I>(_index);
			}
		}
	}

	constexpr auto operator->() {
		if constexpr (Reversed) {
			if constexpr (I == size_t(-1)) {
				return TViewPointer(_container->get(_container->size() - _index - 1));
			} else {
				return TViewPointer(_container->template get<I>(_container->size() - _index - 1));
			}
		} else {
			if constexpr (I == size_t(-1)) {
				return TViewPointer(_container->get(_index));
			} else {
				return TViewPointer(_container->template get<I>(_index));
			}
		}
	}

	constexpr auto operator->() const {
		if constexpr (Reversed) {
			if constexpr (I == size_t(-1)) {
				return TViewPointer(_container->get(_container->size() - _index - 1));
			} else {
				return TViewPointer(_container->template get<I>(_container->size() - _index - 1));
			}
		} else {
			if constexpr (I == size_t(-1)) {
				return TViewPointer(_container->get(_index));
			} else {
				return TViewPointer(_container->template get<I>(_index));
			}
		}
	}

	constexpr TNestedIterator &operator+=(size_t i) {
		_index += i;
		return *this;
	}

	constexpr TNestedIterator &operator-=(size_t i) {
		DEBUG_ASSERT(_index >= i);
		_index -= i;
		return *this;
	}

	friend constexpr TNestedIterator operator+(TNestedIterator Ni, size_t i) {
		Ni._index += i;
		return Ni;
	}

	friend constexpr TNestedIterator operator-(TNestedIterator Ni, size_t i) {
		DEBUG_ASSERT(Ni._index >= i);
		Ni._index -= i;
		return Ni;
	}

	constexpr TNestedIterator &operator++() {
		++_index;
		return *this;
	}

	constexpr auto operator++(int) {
		++_index;
		if constexpr (I > 0) {
			return _container->template get<I>(_index - 1);
		} else {
			return _container->get(_index - 1);
		}
	}

	constexpr TNestedIterator &operator--() {
		DEBUG_ASSERT(_index > 0);
		--_index;
		return *this;
	}

	constexpr auto operator--(int) {
		DEBUG_ASSERT(_index > 0);
		--_index;
		if constexpr (I > 0) {
			return _container->template get<I>(_index + 1);
		} else {
			return _container->get(_index + 1);
		}
	}

	friend constexpr bool operator==(TNestedIterator lhs, TNestedIterator rhs) noexcept {
		return lhs._container == rhs._container && lhs._index == rhs._index;
	}

	friend constexpr bool operator!=(TNestedIterator lhs, TNestedIterator rhs) noexcept {
		return !operator==(lhs, rhs);
	}

	friend constexpr difference_type operator-(TNestedIterator lhs, TNestedIterator rhs) noexcept {
		return lhs._index - rhs._index;
	}
};

} // namespace coretools

#endif
