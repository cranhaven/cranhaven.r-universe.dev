/*
 * Memoize.h
 *
 *  Created on: 23.12.2021
 *      Author: Andreas
 */

#ifndef Memoize_H_
#define Memoize_H_

#include <functional>
#include <map>
#include <tuple>
#include <type_traits>

namespace coretools {

template<typename R, typename... Args> struct TMemoizer {
public:
	TMemoizer(std::function<R(Args...)> fn) : _fn(fn){};

	// Return value if fundamental type, else return const reference
	constexpr std::conditional_t<std::is_fundamental_v<R>, R, const R &> operator()(Args... args) const {
		const auto key = std::make_tuple(args...);
		if (auto it = _lookup.find(key); it != _lookup.end()) return it->second;
		return _lookup.emplace(key, _fn(args...)).first->second;
	}

	void clear() { _lookup.clear(); }
	size_t size() const { return _lookup.size(); }

private:
	mutable std::map<std::tuple<Args...>, R> _lookup;
	std::function<R(Args...)> _fn;
};

// Helper function for ease of use
template<typename R, typename... Args> TMemoizer<R, Args...> memoize(std::function<R(Args...)> fn) {
	return TMemoizer<R, Args...>{fn};
}

// Create std::function of lambda or function pointer and run memoize
// Not possible as a constructor
template<typename Function> auto memoize(Function fn) { return memoize(std::function(fn)); }

} // namespace coretools

#endif
