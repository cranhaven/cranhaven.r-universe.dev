#ifndef CORETOOLS_CONTAINERS_TLAZYITERATOR_H
#define CORETOOLS_CONTAINERS_TLAZYITERATOR_H

#include "coretools/Main/TError.h"
#include <iterator>

namespace coretools {

template<typename LazyContainer> class TLazyIterator {
	LazyContainer *_container;

public:
	using value_type        = typename LazyContainer::value_type;
	using reference         = typename LazyContainer::const_reference;
	using iterator_category = std::input_iterator_tag;
	using difference_type   = std::ptrdiff_t;
	using pointer           = void;

	constexpr TLazyIterator(LazyContainer *Container = nullptr) : _container(Container) {}

	constexpr reference operator*() const noexcept(noDebug) {
		DEBUG_ASSERT(_container);
		return _container->front();
	}

	constexpr TLazyIterator &operator++() noexcept(noDebug) {
		DEBUG_ASSERT(_container);
		_container->popFront();
		return *this;
	}

	constexpr value_type operator++(int) noexcept(noDebug) {
		DEBUG_ASSERT(_container);
		const value_type r = _container->front();
		_container->popFront();
		return r;
	}

	// Does not actually do any comparison. Only used to check for end
	constexpr bool operator!=(TLazyIterator) const noexcept(noDebug) {
		DEBUG_ASSERT(_container);
		return !_container->empty();
	}
};

} // namespace coretools

#endif
