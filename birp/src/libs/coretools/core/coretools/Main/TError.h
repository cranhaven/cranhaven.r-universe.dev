//
// Created by madleina on 14.04.22.
//

#ifndef TERROR_H
#define TERROR_H

#include "coretools/Strings/toString.h"

namespace coretools::err {

template<bool UserError> class TError : public std::exception {
	// Custom error class
	// mimics interface of std::exception
	// template argument is used to recognize which error was thrown: user or developer error. This is needed in TMain.h
	// inherits public from std::exception to simplify catches: catching std::exception by reference will also catch TError
private:
	std::string _what;
	std::string _location;
	std::string _error;
public:
	explicit TError(std::string Location, std::string Error) : _location(std::move(Location)), _error(std::move(Error)) {
		if constexpr (UserError)
			_what = _error;
		else
			_what = _location + _error;
	}
	explicit TError(const TError<!UserError>& other) : TError(other._location, other._error) {}

	[[nodiscard]] const char *what() const noexcept override { return _what.c_str(); }
	std::string location() const noexcept {return _location; }
	std::string error() const noexcept {return _error; }
};

using TUserError = TError<true>;
using TDevError  = TError<false>;

} // namespace coretools::err

// Macro that throws a user error
#define UERROR(...)                                                                                                    \
	throw coretools::err::TUserError(                                                                                  \
	    coretools::str::toString("File ", __FILE__, ", line ", __LINE__, ", function ", __PRETTY_FUNCTION__, ": "), \
	    coretools::str::toString(__VA_ARGS__))

// Macro that throws a developer error
#define DEVERROR(...)                                                                                                  \
	throw coretools::err::TDevError(                                                                                   \
		coretools::str::toString("File ", __FILE__, ", line ", __LINE__, ", function ", __PRETTY_FUNCTION__, ": "), \
	    coretools::str::toString(__VA_ARGS__))

#endif // TERROR_H
