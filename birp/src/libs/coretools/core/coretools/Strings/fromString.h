/* fromString.h
 *
 *  Created on: Sep 27, 2022
 *      Author: Andreas
 */

#ifndef CORE_STR_FROMSTRING_H
#define CORE_STR_FROMSTRING_H

#include <algorithm>
#include <charconv>
#include <cmath>
#include <string>
#include <string_view>
#include <type_traits>

#include "fast_float/fast_float.h"

#include "coretools/Main/TError.h"
#include "coretools/Strings/splitters.h"
#include "coretools/traits.h"

namespace coretools::str {

template<bool Check=false, typename T>
void fromString(std::string_view from, T& to) noexcept(!Check);

template<typename T, bool Check = false>
T fromString(std::string_view from) noexcept(!Check);

namespace impl {
	template<bool Check>
	void fromStringBool(std::string_view from, bool& to) noexcept(!Check) {
		if constexpr (Check) {
			if (from == "true" || from == "1")
				to = true;
			else if (from == "false" || from == "0")
				to = false;
			else
				UERROR("Can not convert string '", from, "' to a boolean!");
		} else {
			to = (from == "true" || from == "1");
		}
	}
	template<bool Check, typename T>
	void fromStringInt(std::string_view from, T& to) noexcept(!Check) {
		if constexpr (Check) {
			const auto a = std::from_chars(from.data(), from.data() + from.size(), to);
			if (a.ec == std::errc::invalid_argument) { UERROR("String '", from, "' is not a number!"); }
			if (a.ec == std::errc::result_out_of_range) { UERROR("String '", from, "' is out of range!"); }
			if (a.ptr != from.data() + from.size()) { UERROR("String '", from, "' contains superfluous characters!"); }
		} else {
			std::from_chars(from.data(), from.data() + from.size(), to);
		}
	}
	template<bool Check, typename T>
	void fromStringFloat(std::string_view from, T& to) noexcept(!Check) {
		if constexpr (Check) {
			const auto a = fast_float::from_chars(from.data(), from.data() + from.size(), to);
			if (a.ec == std::errc::invalid_argument) {
				UERROR("String '", from, "' is not a number!");
			}
			if (a.ec == std::errc::result_out_of_range) {
				UERROR("String '", from, "' is out of range!");
			}
			if (a.ptr != from.data() + from.size()) {
				UERROR("String '", from, "' contains superfluous characters!");
			}
			if (!std::isfinite(to)) {
				UERROR("String '", to, "' is not a number!");
			}

		} else {
			fast_float::from_chars(from.data(), from.data() + from.size(), to);
		}
	}

	template<bool Check>
	size_t removeRepeat(std::string_view& s) {
		if (s.back() != '}') return 1; // only 1 repetition
		// changes input!

		s.remove_suffix(1); // remove '}'
		const auto nStart = s.rfind('{');
		const auto n      = fromString<size_t, Check>(s.substr(nStart + 1));
		s                 = s.substr(0, nStart);
	    return n;
    }

    template<bool Check, typename T> void fromStringIterable(std::string_view from, T &to) noexcept(!Check) {
		if constexpr (isResizable_v<T>) to.resize(0);
		if (from.front() == '(' || from.front() == '[' || from.front() == '{') {
			if constexpr (Check) {
				const auto f = from.front();
				const auto b = from.back();
				if ((f == '(' && b != ')') || (f == '[' && b != ']') || (f == '{' && b != '}'))
					UERROR("Starting and ending parens are not the same in ", from, "!");
		    }
		    from.remove_prefix(1);
			from.remove_suffix(1);
		}
		constexpr std::string_view delims = ",;| \t\n";
		const auto it                     = std::find_first_of(from.begin(), from.end(), delims.begin(), delims.end());
		const auto delim                  = it == from.end() ? '\0' : *it;
	    TSplitter spl(from, delim);
	    size_t i = 0;
		for (auto s : spl) {
			const auto n   = removeRepeat<Check>(s) + i;
			const auto val = fromString<typename T::value_type, Check>(s);

			for (; i < n; ++i) {
				if constexpr (isResizable_v<T>) {
					to.push_back(val);
				} else {
					if constexpr (Check) {
						if (i >= to.size()) UERROR("String ", from, " contains too many values!");
					}
					to[i] = val;
				}
			}
		}
	}
	} // namespace impl

template<bool Check, typename T>
void fromString(std::string_view from, T& to) noexcept(!Check) {
	if constexpr (std::is_same_v<T, std::string_view>) {
		to = from;
	} else if constexpr (std::is_same_v<T, std::string>) {
		to = std::string(from);
	} else if constexpr (std::is_same_v<T, bool>) {
		impl::fromStringBool<Check>(from, to);
	} else if constexpr (std::is_integral_v<T>) {
		impl::fromStringInt<Check>(from, to);
	} else if constexpr (std::is_floating_point_v<T>) {
		impl::fromStringFloat<Check>(from, to);
	} else if constexpr (hasGetMember_v<T>) {
		underlyingType_t<T> val{};
		fromString<Check>(from, val);
		to = static_cast<T>(val);
	} else if constexpr (std::is_constructible_v<T, std::string_view>) {
		if constexpr (Check){
			to = T(from);
		} else {
			to = T(tags::NoCheck{}, from);
		}
	} else if constexpr (std::is_constructible_v<T, std::string>) {
		if constexpr (Check){
			to = T(std::string(from));
		} else {
			to = T(tags::NoCheck{}, std::string(from));
		}
	} else if constexpr (isIterable_v<T>) {
		impl::fromStringIterable<Check>(from, to);
	} else {
		static_assert(!sizeof(T), "Cannot convert");
	}
}

template<typename T, bool Check>
T fromString(std::string_view from) noexcept(!Check) {
	T to{}; // use default constructor
	fromString<Check>(from, to);
	return to;
}

template<typename T>
void fromStringCheck(std::string_view from, T& to) {
	fromString<false>(from, to);
}

template<typename T>
T fromStringCheck(std::string_view from) {
	return fromString<T, true>(from);
}

template<typename T>
void fromString(std::string_view from, T& to, std::string_view errorMessage) {
	try{
		fromString<true>(from, to);
	}
	catch(coretools::err::TUserError & error){
		throw coretools::err::TUserError("", (std::string) errorMessage + error.error());
	}
	catch(coretools::err::TDevError & error){
		throw error;
	}
}

}

#endif
