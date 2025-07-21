
#ifndef CONVERTSTRING_H_
#define CONVERTSTRING_H_

#include "coretools/Main/TError.h"
#include "coretools/Strings/stringConversions.h"
#include "coretools/Strings/stringManipulations.h"
#include "coretools/Strings/fromString.h"

#include <string_view>

namespace coretools::str {
//-------------------------------------------------
// Convert multiple parameters
//-------------------------------------------------

namespace impl {
template<class Type>
void convertOneParam(std::string_view String, std::string_view Explanation, size_t Counter, Type &Val) {
	// convert
	try {
		fromString<true>(strip(String), Val);
	} catch (err::TError &error) {
		if (error.isDevError()) {
			throw TDevError(error.what());
		} else {
			throw TUserError("Failed to parse the ", intToNumeralAdjective(Counter + 1), " parameter: ", error.what(),
							 " ", Explanation);
		}
	}
}

inline void convertString(TSplitter<> Spl, std::string_view Explanation, size_t Counter, size_t ExpectedNumArgs) {
	// termination version
	// check if we've used entire parameter string
	user_assert(Spl.empty(), "More parameters (", Counter + 1, ") than expected (", ExpectedNumArgs, ")! ", Explanation);
}

template<class Type, class... Types>
void convertString(TSplitter<> Spl, std::string_view Explanation, size_t Counter, size_t ExpectedNumArgs, Type &Val,
				   Types &...Other) {
	// fill first value from string
	user_assert(!Spl.empty(), "Missing parameter: Found ", Counter, ", expected ", ExpectedNumArgs, " parameters. ", Explanation);

	impl::convertOneParam(Spl.front(), Explanation, Counter, Val);
	Spl.popFront();
	// recursive call with other values
	impl::convertString(Spl, Explanation, Counter + 1, ExpectedNumArgs, Other...);
}

} // namespace impl

template<class Type, class... Types>
void convertString(std::string_view String, std::string_view Explanation, Type &Val, Types &...Other) {
	// this function is called by developer: counter = 0
	constexpr size_t expectedNumArgs = sizeof...(Types) + 1;
	impl::convertString(TSplitter(String, ','), Explanation, 0, expectedNumArgs, Val, Other...);
}

inline void convertString(std::string_view String, std::string_view Explanation) {
	// used if a distribution doesn't expect any parameters
	// this function is called by developer: counter = 0
	impl::convertString(TSplitter(String, ','), Explanation, 0, 0);
}

} // namespace coretools::str

#endif
