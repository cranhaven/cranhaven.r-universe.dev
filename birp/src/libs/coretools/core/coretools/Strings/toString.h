#ifndef TOSTRING_H
#define TOSTRING_H

#include <cstdio>
#include <string>
#include <type_traits>
#include <vector>
#include <string_view>

#ifndef USE_RCPP
#include "fmt/ranges.h"
#include "fmt/format.h"
constexpr bool HASFMT = true;
#else
constexpr bool HASFMT = false;
namespace fmt{
template<typename T>
std::string format(std::string_view, const T& t) {
	char buf[256];
	if constexpr (std::is_floating_point_v<T>) snprintf(buf, sizeof(buf), "%g", static_cast<double>(t));
	else snprintf(buf, sizeof(buf), "%d", static_cast<int>(t));
	return buf;
}
}
#endif
#include "coretools/traits.h"

//-------------------------------------------------
// Conversion to string
//-------------------------------------------------

namespace coretools::str {

inline std::string toString() { return std::string{}; }

inline const std::string & toString(const std::string &s) { return s; }

inline std::string toString(char c) { return std::string{1, c}; }

inline std::string toString(std::string_view sv) {
	return std::string(sv);
}

template<typename T> std::string toString(const T &val) {
	if constexpr (std::is_same_v<T, bool> || std::is_same_v<T, std::vector<bool>::reference> ||
				  std::is_same_v<T, std::vector<bool>::const_reference>) {
		constexpr const char *ret[2] = {"false", "true"};
		return ret[val];
	} else if constexpr (std::is_integral_v<T>) {
		return fmt::format("{:}", val);
	} else if constexpr (std::is_floating_point_v<T>) {
		return fmt::format("{:.6}", val);
	} else if constexpr (isIterable_v<T>) {
		if (val.begin() == val.end()) { return "[]"; }
		if constexpr (HASFMT && std::is_fundamental_v<typename T::value_type>) {
			// can be done by fmt
			return fmt::format("{}", val);
		} else {
			// we need to do this, have at least one element
			std::string s("[");
			s.append(toString(*val.begin()));
			s.reserve((s.size() + 1) * std::distance(val.begin(), val.end()) + 1);
			for (auto it = val.begin() + 1; it < val.end(); ++it) {
				s.append(", ").append(toString(*it));
			}
			s.append("]");
			return s;
		}
	} else {
		return static_cast<std::string>(val);
	}
}

template<typename T1, typename T2, typename... Ts> std::string toString(T1 &&Val1, T2 &&Val2, Ts &&...Values) {
	return toString(std::forward<T1>(Val1)) + toString(std::forward<T2>(Val2)) + toString(std::forward<Ts>(Values)...);
}

} // namespace coretools::str

#endif // TOSTRING_H
