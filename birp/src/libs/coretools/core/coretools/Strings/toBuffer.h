//
// Created by andreas on 19.04.22.
//

#ifndef TOBUFFER_H
#define TOBUFFER_H

#include <string>
#include <vector>
#include <string_view>

#ifndef USE_RCPP
#include "fmt/ranges.h"
#include "fmt/format.h"
#else
#include "toString.h"
namespace coretools::str {
template<typename OutputIt> constexpr OutputIt toBuffer(OutputIt out, std::string_view s);
}
namespace fmt{
template<typename OutputIt, typename T>
OutputIt format_to(OutputIt out, std::string_view, const T& t) {
	return coretools::str::toBuffer(out, coretools::str::toString(t));
}	
}
#endif
#include "coretools/traits.h"

namespace coretools::str {

template<typename OutputIt>
constexpr OutputIt toBuffer(OutputIt out) { return out; }

template<typename OutputIt> constexpr OutputIt toBuffer(OutputIt out, char c) {
	*out = c;
	++out;
	return out;
}

template<typename OutputIt> constexpr OutputIt toBuffer(OutputIt out, std::string_view s) {
	std::copy(s.begin(), s.end(), out);
	return out;
}
template<typename OutputIt, typename T>
OutputIt toBuffer(OutputIt out, const T &val) {
	if constexpr (std::is_same_v<T, bool> || std::is_same_v<T, std::vector<bool>::reference> ||
				  std::is_same_v<T, std::vector<bool>::const_reference>) {
		constexpr std::string_view ret[2] = {"false", "true"};
		return toBuffer(out, ret[val]);
	} else if constexpr (std::is_integral_v<T>) {
		return fmt::format_to(out, "{:}", val);
	} else if constexpr (std::is_floating_point_v<T>) {
		return fmt::format_to(out, "{:.6}", val);
	} else if constexpr (isIterable_v<T>) {
		return fmt::format_to(out, "{}", val);
	} else {
		std::string s = static_cast<std::string>(val);
		std::copy(s.begin(), s.end(), out);
		return out;
	}
}

template<typename OutputIt, typename T1, typename T2, typename... Ts>
OutputIt toBuffer(OutputIt out, T1&& Val1, T2&& Val2, Ts&& ...Values) {
	toBuffer(out, std::forward<T1>(Val1));
	toBuffer(out, std::forward<T2>(Val2));
	return toBuffer(out, std::forward<Ts>(Values)...);
}
}
#endif
