
#ifndef STRINGMANIPULATIONS_H_
#define STRINGMANIPULATIONS_H_

#include <string>
#include "coretools/Strings/stringConstants.h"

namespace coretools::str {

//-------------------------------------------------
// Erase, extract and trim
//-------------------------------------------------
std::string stringReplace(char Needle, std::string_view Replace, std::string_view Haystack);
std::string stringReplace(std::string_view Needle, std::string_view Replace, std::string_view Haystack);

void eraseAllOccurences(std::string &s, std::string_view needle, bool any = false);
void eraseAllOccurences(std::string &s, char needle);

void eraseAllWhiteSpaces(std::string &s);
void eraseAllNonAlphabeticCharacters(std::string &s);

// read
std::string_view readBefore(std::string_view s, std::string_view needle, bool any = false);
std::string_view readBefore(std::string_view s, char needle);

std::string_view readUntil(std::string_view s, std::string_view needle, bool any = false);
std::string_view readUntil(std::string_view s, char needle);

std::string_view readBeforeLast(std::string_view s, std::string_view needle, bool any = false);
std::string_view readBeforeLast(std::string_view s, char needle);

std::string_view readUntilLast(std::string_view s, std::string_view needle, bool any = false);
std::string_view readUntilLast(std::string_view s, char needle);

std::string_view readAfter(std::string_view s, std::string_view needle, bool any = false);
std::string_view readAfter(std::string_view s, char needle);

std::string_view readAfterLast(std::string_view s, std::string_view needle, bool any = false);
std::string_view readAfterLast(std::string_view s, char needle);

std::string_view readBetween(std::string_view s, std::string_view  left, std::string_view  right, bool any = false);
std::string_view readBetween(std::string_view s, char left, char right);

// manipulations
std::string extractBefore(std::string &s, std::string_view needle, bool any = false);
std::string extractBefore(std::string &s, char needle);

std::string extractUntil(std::string &s, std::string_view needle, bool any = false);
std::string extractUntil(std::string &s, char needle);

std::string extractBeforeLast(std::string &s, std::string_view needle, bool any = false);
std::string extractBeforeLast(std::string &s, char needle);

std::string extractUntilLast(std::string &s, std::string_view needle, bool any = false);
std::string extractUntilLast(std::string &s, char needle);

std::string extractAfter(std::string &s, std::string_view needle, bool any = false);
std::string extractAfter(std::string &s, char needle);

std::string extractAfterLast(std::string &s, std::string_view needle, bool any = false);
std::string extractAfterLast(std::string &s, char needle);

std::string extractBetween(std::string &s, std::string_view left, std::string_view right, bool any = false);
std::string extractBetween(std::string &s, char left, char right);

std::string extractPath(std::string &s);
std::string extractBeforeDoubleSlash(std::string &s);
std::string extractBeforeWhiteSpace(std::string &s);

// trim
void trimString(std::string &s, std::string_view what = " \t\f\v\n\r");
void trimEndlineString(std::string &s);

template<bool any>
std::pair<std::string_view, std::string_view> split(std::string_view sv, std::string_view delim) {
	if constexpr (any) {
		const auto p = sv.find_first_of(delim);
		if (p == std::string_view::npos) return {sv, ""};
		return {sv.substr(0, p), sv.substr(p+1)};
	} else {
		const auto p = sv.find(delim);
		if (p == std::string_view::npos) return {sv, ""};
		return {sv.substr(0, p), sv.substr(p + delim.size())};
	}
}

std::string split(std::string &s, std::string_view delim, bool any = true);
std::string split(std::string &s, char delim);

std::string splitAtLast(std::string &s, std::string_view delim, bool any = true);
std::string splitAtLast(std::string &s, char delim);

std::string splitAtPos(std::string &s, std::string::size_type l, size_t Length = 1);

//capitalize
std::string capitalizeFirst(std::string s);

constexpr std::string_view lstrip(std::string_view sv, std::string_view pattern = whitespacesPlus) noexcept {
	const auto start = std::min(sv.find_first_not_of(pattern), sv.size());
	return sv.substr(start);
}

constexpr std::string_view rstrip(std::string_view sv, std::string_view pattern = whitespacesPlus) noexcept {
	return sv.substr(0, sv.find_last_not_of(pattern) + 1);
}

constexpr std::string_view strip(std::string_view sv, std::string_view pattern = whitespacesPlus) noexcept {
	const auto start = std::min(sv.find_first_not_of(pattern), sv.size());
	const auto end   = sv.find_last_not_of(pattern) + 1;
	return sv.substr(start, end - start);
}


std::string camelCase(std::string_view Phrase);
std::string snake_case(std::string_view Phrase);

}
#endif
