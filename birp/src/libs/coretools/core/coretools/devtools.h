/*
 * devtools.h
 *
 *  Created on: Oct 14, 2021
 *      Author: andreas
 */

#ifndef DEVTOOLS_H_
#define DEVTOOLS_H_

#ifdef DEVTOOLS

#include <chrono>
#include <string>

#include "coretools/Strings/toString.h"
#include "coretools/macros.h"

namespace coretools::devtools {

inline std::string &currentFunction() {
	static std::string function;
	return function;
}

template<typename T, typename... Ts>
inline std::string comafy(T t, Ts... ts) noexcept {
	using coretools::str::toString;
	if constexpr (sizeof...(ts) == 0) {
		return toString(t);
	} else {
		return toString(t, ", ") + comafy(ts...);
	}
}

template<typename... Ts>
inline std::string appendValues(std::string_view Text, Ts... ts) noexcept {
	using coretools::str::toString;
	if constexpr (sizeof...(ts) == 0) {
		return std::string{Text};
	} else {
		return toString(Text, " = ", comafy(ts...));
	}
}

template<typename... Ts>
inline void devOut(std::string_view Location, std::string_view Function, std::string_view Text, Ts... ts) noexcept {
	if (Function != currentFunction()) {
		currentFunction() = Function;
		fmt::print(stderr, "***DEBUG INFO*** In function {}\n", currentFunction());
	}
	fmt::print(stderr, "{}{}\n", Location, appendValues(Text, ts...));
}

class TScopedTimer {
private:
	std::string _location;
	std::string _function;
	std::string _name = "ScopedTimer";
	std::chrono::time_point<std::chrono::high_resolution_clock> _start;

public:
	TScopedTimer(std::string_view location, std::string_view function)
		: _location(location), _function(function), 
		  _start(std::chrono::high_resolution_clock::now()){}

	TScopedTimer(std::string_view location, std::string_view function, std::string_view name)
		: _location(location), _function(function), 
		  _start(std::chrono::high_resolution_clock::now()){
		_name.append("(").append(name).append(")");
	}

	~TScopedTimer() {
		using namespace std::chrono;
		using coretools::str::toString;
		const duration<double> dt = high_resolution_clock::now() - _start;
		devOut(_location, _function, _name + " took " + toString(dt.count()) + "s");
	}
};

} // namespace coretools::devtools

#ifndef DEV_NO_LOCATION
#define __FILE_LINE__ __FILE__ ":" STRINGIFY(__LINE__) ": "
#else
#define __FILE_LINE__ "\t"
#endif

#define OUT(...) coretools::devtools::devOut(__FILE_LINE__, __PRETTY_FUNCTION__, STRINGIFY(__VA_ARGS__), __VA_ARGS__)

#define ECHO(s) coretools::devtools::devOut(__FILE_LINE__, __PRETTY_FUNCTION__, s)
#define WINK() ECHO(";-)")

#define SCOPEDTIMER(...) coretools::devtools::TScopedTimer MERGE(__unique__, __LINE__)(__FILE_LINE__, __PRETTY_FUNCTION__ __VA_OPT__(,) __VA_ARGS__)

// Use unique_pointer, if TOC is forgotten, memory is not leaked
#define TIC() auto __tic__ = std::make_unique<coretools::devtools::TScopedTimer>(__FILE_LINE__, __PRETTY_FUNCTION__, "TicToc");
#define TOC() delete __tic__.release();

#else // no DEVTOOLS
#define OUT(...)
#define ECHO(s)
#define WINK()
#define SCOPEDTIMER(...)
#define TIC()
#define TOC()

#endif // DEVTOOLS
#endif // DEVTOOLS_H_
