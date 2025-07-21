#ifndef CORETOOLS_TSOURCELOCATION_H_
#define CORETOOLS_TSOURCELOCATION_H_

#include "coretools/Strings/toString.h"
#include <cstddef>
#include <string_view>

namespace coretools {

// use std::source_location in C++20: https://en.cppreference.com/w/cpp/utility/source_location
class TSourceLocation {
	size_t _line;
	std::string_view _fileName;
	std::string_view _functionName;

public:
	constexpr TSourceLocation(size_t Line, std::string_view FileName, std::string_view FunctionName)
	    : _line(Line), _fileName(FileName), _functionName(FunctionName) {}
	constexpr TSourceLocation(const TSourceLocation &SL)
		: TSourceLocation(SL._line, SL._fileName, SL._functionName) {}

	constexpr size_t line() const noexcept {return _line;}
	constexpr std::string_view fileName() const noexcept {return _fileName;}
	constexpr std::string_view functionName() const noexcept {return _functionName;}
	explicit operator std::string() const {
		return str::toString("File ", fileName(), ", line ", line(), ", in function ", functionName());
	}

	static constexpr TSourceLocation current(size_t Line                   = __builtin_LINE(),
											 std::string_view FileName     = __builtin_FILE(),
											 std::string_view FunctionName = __builtin_FUNCTION()) noexcept {
		return TSourceLocation(Line, FileName, FunctionName);
	}
};
}


#endif
