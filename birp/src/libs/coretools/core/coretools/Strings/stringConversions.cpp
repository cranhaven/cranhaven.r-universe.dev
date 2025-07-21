#include "stringConversions.h"
#include "coretools/Main/TError.h"

#include <ctime>

namespace coretools::str {
namespace impl {
std::string numericToAlphabet(int Index, char Start) {
	// function to convert a numeric index to an alphabet string
	// corresponds to column names in Excel
	// example for Start = 'a':
	// 0 -> "a"
	// 1 -> "b"
	// ...
	// 25 -> "z"
	// 26 -> "aa"
	// 27 -> "ab"
	// ...
	int dividend = Index + 1;
	std::string result;
	int modulo;

	while (dividend > 0) {
		modulo   = (dividend - 1) % 26;
		result   = static_cast<char>(Start + modulo) + result;
		dividend = static_cast<int>((dividend - modulo) / 26);
	}

	return result;
}

size_t alphabetIndexToNumeric(std::string_view Input, char Start) {
	// reverse function of _numericToAlphabet
	DEV_ASSERT(!Input.empty());

	int result = 0;
	for (const auto &character : Input) {
		result *= 26;
		result += character - Start + 1;
	}
	return result - 1;
}
} // namespace impl

std::string timeToString(const time_t &time, bool local) {
	struct tm *timeinfo;
	char buffer[20];

	if (local)
		timeinfo = localtime(&time);
	else
		timeinfo = gmtime(&time);

	strftime(buffer, 20, "%Y-%m-%d %H:%M:%S", timeinfo);
	return std::string(buffer);
};

std::string numericToLowerCaseAlphabetIndex(int Index) { return impl::numericToAlphabet(Index, 'a'); }
std::string numericToUpperCaseAlphabetIndex(int Index) { return impl::numericToAlphabet(Index, 'A'); }
size_t lowerCaseAlphabetIndexToNumeric(std::string_view Input) { return impl::alphabetIndexToNumeric(Input, 'a'); }
size_t upperCaseAlphabetIndexToNumeric(std::string_view Input) { return impl::alphabetIndexToNumeric(Input, 'A'); }

} // namespace coretools::str
