
#ifndef STRINGCONVERSIONS_H_
#define STRINGCONVERSIONS_H_

#include <string_view>
#include <string>

#include "toString.h"

#ifndef USE_RCPP
#include "fmt/format.h"
#else
#include <iomanip>
#include <sstream>
namespace fmt {
template<typename T> std::string format(std::string_view, const T &t, int precision) {
	std::stringstream stream;
	stream << std::fixed << std::setprecision(precision) << t;
	return stream.str();
}
}
#endif

namespace coretools::str {


// with specific precision
template<typename T> std::string toStringWithPrecision(T val, int precision = 6) {
	static_assert(std::is_floating_point_v<T>);
	return fmt::format("{:.{}}", val, precision);
};

// as percent string (e.g. used to report progress)
template<typename T> std::string toPercentString(T input, int precision = 0) {
	static_assert(std::is_arithmetic_v<T>);
	return toStringWithPrecision(input * 100.0, precision);
};

template<typename T> std::string toPercentString(T num, T total, int precision = 0) {
	static_assert(std::is_integral_v<T>);
	return toStringWithPrecision(static_cast<double>(num)/total*100.0, precision);
};

// number to numeral adjectives (first, second, ...)
template<typename T> std::string intToNumeralAdjective(T Number) {
	static_assert(std::is_integral_v<T>);
	if (Number == 1) {
		return "first";
	} else if (Number == 2) {
		return "second";
	} else if (Number == 3) {
		return "third";
	} else {
		return toString(Number, "th");
	}
}

std::string timeToString(const time_t &time, bool local);

std::string numericToLowerCaseAlphabetIndex(int Index);
std::string numericToUpperCaseAlphabetIndex(int Index);
size_t lowerCaseAlphabetIndexToNumeric(std::string_view Input);
size_t upperCaseAlphabetIndexToNumeric(std::string_view Input);

}

#endif
