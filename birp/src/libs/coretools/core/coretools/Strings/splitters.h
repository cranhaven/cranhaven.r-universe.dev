#ifndef SPLITTERS_H_
#define SPLITTERS_H_

#include <string>
#include <type_traits>
#include <cassert>

#include "coretools/Containers/TLazyIterator.h"
#include "coretools/Strings/stringConstants.h"

namespace coretools::str {

template<typename Delim = char, bool Any = false>
class TSplitter {
	static constexpr bool isChar = std::is_same_v<Delim, char>;
	static_assert((Any && !isChar) || !Any);
	std::string_view _sv;
	Delim _delim;
	size_t _count;

	constexpr static size_t _find(std::string_view s, Delim _delim) noexcept {
		if constexpr (Any) {
			return s.find_first_of(_delim);
		} else {
			return s.find(_delim);
		}
	}

public:
	using iterator        = TLazyIterator<TSplitter>;
	using value_type      = std::string_view;
	using const_reference = value_type;

	TSplitter(std::string_view Sv, Delim delim) : _sv(Sv), _delim(std::move(delim)), _count(_find(_sv, _delim)) {}

	constexpr bool empty() const noexcept { return _sv.empty(); }

	constexpr const_reference front() const noexcept {
		assert(!empty());
		return _sv.substr(0, _count);
	}

	constexpr void popFront() noexcept {
		assert(!empty());
		if (_count == std::string_view::npos) {
			_sv.remove_prefix(_sv.size());
		} else {
			if constexpr (isChar || Any) {
				_sv.remove_prefix(_count + 1);
			} else {
				_sv.remove_prefix(_count + _delim.size());
			}
			_count = _find(_sv, _delim); // will be npos for last element
		}
	}

	constexpr iterator begin() noexcept {return iterator{this};}
	constexpr iterator end() noexcept {return iterator{};}
};

template<typename Delim = char, bool Any = false>
class TReverseSplitter {
	static constexpr bool isChar = std::is_same_v<Delim, char>;
	static_assert((Any && !isChar) || !Any);
	static_assert(isChar || std::is_same_v<Delim, std::string> ||
				  std::is_same_v<Delim, std::string_view>);
	std::string_view _sv;
	Delim _delim;
	size_t _start;

	constexpr static size_t _find(std::string_view s, Delim _delim) noexcept {
		if constexpr (Any) {
			return s.find_last_of(_delim) + 1;
		} else if constexpr (isChar) {
			// std::string_view::npos + 1 = 0
			return s.rfind(_delim) + 1;
		} else {
			const auto p = s.rfind(_delim);
			if (p == std::string_view::npos) return 0;
			return p + _delim.size();
		}
	}

public:
	using iterator        = TLazyIterator<TReverseSplitter>;
	using value_type      = std::string_view;
	using const_reference = value_type;

	TReverseSplitter(std::string_view Sv, Delim delim) : _sv(Sv), _delim(std::move(delim)), _start(_find(_sv, _delim)) {}

	bool empty() const noexcept { return _sv.empty(); }

	std::string_view front() const noexcept {
		assert(!empty());
		return _sv.substr(_start);
	}

	void popFront() noexcept {
		assert(!empty());

		if (_start == 0) {
			_sv = _sv.substr(0, 0);
		} else {
			if constexpr (isChar || Any) {
				_sv.remove_suffix(_sv.size() - _start + 1);
			} else {
				_sv.remove_suffix(_sv.size() - _start + _delim.size());
			}
			_start = _find(_sv, _delim);
		}
	}

	constexpr iterator begin() noexcept {return iterator{this};}
	constexpr iterator end() noexcept {return iterator{};}
};

class TWhitespaceSplitter {
	static constexpr std::string_view _delim = whitespaces;
	std::string_view _sv;
	size_t _count;

public:
	using iterator        = TLazyIterator<TWhitespaceSplitter>;
	using value_type      = std::string_view;
	using const_reference = value_type;

	TWhitespaceSplitter(std::string_view Sv) : _sv(Sv.substr(Sv.find_first_not_of(_delim))), _count(_sv.find_first_of(_delim)) {}

	constexpr bool empty() const noexcept { return _sv.empty(); }

	constexpr const_reference front() const noexcept {
		assert(!empty());
		return _sv.substr(0, _count);
	}

	constexpr void popFront() noexcept {
		assert(!empty());
		if (_count == std::string_view::npos) {
			_sv.remove_prefix(_sv.size());
		} else {
			_sv.remove_prefix(_count);
			_sv.remove_prefix(_sv.find_first_not_of(_delim)); // remove all whitespaces
			_count = _sv.find_first_of(_delim); // will be npos for last element
		}
	}

	constexpr iterator begin() noexcept {return iterator{this};}
	constexpr iterator end() noexcept {return iterator{};}
};

template<typename LazyContainer> void skip(LazyContainer &container, size_t nGaps = 1) noexcept {
	for (size_t _ = 0; _ < nGaps; ++_) container.popFront();
}

} // namespace coretools::str

#endif
