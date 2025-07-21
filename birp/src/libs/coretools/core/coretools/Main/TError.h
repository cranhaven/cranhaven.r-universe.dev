//
// Created by madleina on 14.04.22.
//

#ifndef TERROR_H
#define TERROR_H

#include "coretools/Strings/toString.h"
#include "coretools/TSourceLocation.h"
#include "coretools/macros.h"

namespace coretools {
namespace err {

class TError : public std::exception {
	// inherits public from std::exception to simplify catches: catching std::exception by reference will also catch TError
private:
	TSourceLocation _location;
	std::string _error;
public:
	explicit TError(const TSourceLocation &Loc, std::string_view Error) : _location(Loc), _error(Error) {}

	[[nodiscard]] const char *what() const noexcept override { return _error.c_str(); }
	const TSourceLocation& location() const noexcept {return _location; }
	virtual bool isDevError() noexcept = 0;
};
}

template<typename T, typename... Ts>
struct TUserError : public err::TError {
	TUserError(T &&t, Ts &&...ts, const TSourceLocation &Loc = TSourceLocation::current()) :
		TError(Loc, str::toString(t, ts...)) {}
	bool isDevError() noexcept override {return false;}
};
template<typename T, typename... Ts> TUserError(T &&, Ts &&...) -> TUserError<T, Ts...>;

template<typename T, typename... Ts>
struct TDevError : public err::TError {
	TDevError(T &&t, Ts &&...ts, const TSourceLocation &Loc = TSourceLocation::current()) :
		TError(Loc, str::toString(t, ts...)) {}
	bool isDevError() noexcept override {return true;}
};
template<typename T, typename... Ts> TDevError(T &&, Ts &&...) -> TDevError<T, Ts...>;


// Assertions
#ifdef NDEBUG
constexpr bool noDebug = true;
#else
constexpr bool noDebug = false;
#endif

// CTAD - trick: https://en.cppreference.com/w/cpp/language/class_template_argument_deduction
template<typename T, typename... Ts> struct debug_assert {
	constexpr debug_assert(bool Assertion, T&& t, Ts&&... ts,
							 const TSourceLocation &Loc = TSourceLocation::current()) noexcept(noDebug) {
		if constexpr (!noDebug) {
			if (!Assertion) throw TDevError(Loc, t, ts...);
		}
	}
};
template<typename T, typename... Ts> debug_assert(bool, T&&, Ts&&...) -> debug_assert<T, Ts...>;

template<typename T, typename... Ts> struct dev_assert {
	constexpr dev_assert(bool Assertion, T &&t, Ts &&...ts,
				  const TSourceLocation &Loc = TSourceLocation::current()) {
		if (!Assertion) throw TDevError(Loc, t, ts...);
	}
};
template<typename T, typename... Ts> dev_assert(bool, T&&, Ts&&...) -> dev_assert<T, Ts...>;

template<typename T, typename... Ts> struct user_assert {
	constexpr user_assert(bool Assertion, T &&t, Ts &&...ts,
				  const TSourceLocation &Loc = TSourceLocation::current()) {
		if (!Assertion) throw TUserError(Loc, t, ts...);
	}
};
template<typename T, typename... Ts> user_assert(bool, T&&, Ts&&...) -> user_assert<T, Ts...>;

} // namespace coretools

#define DEBUG_ASSERT(ASSERTION) coretools::debug_assert((ASSERTION), "Debug Assertion '", STRINGIFY(ASSERTION), "' failed!")
#define DEV_ASSERT(ASSERTION) coretools::dev_assert((ASSERTION), "Runtime Assertion '", STRINGIFY(ASSERTION), "' failed!")

// Deprecated macros that throws a user error
#define UERROR(...) _Pragma("message \"UERROR is deprecated, throw coretools::TUserError instead!\"") throw coretools::TUserError(__VA_ARGS__)
#define DEVERROR(...) _Pragma("message \"DEVERROR deprecated, throw coretools::TDevError instead!\"") throw coretools::TDevError(__VA_ARGS__)

#endif // TERROR_H
