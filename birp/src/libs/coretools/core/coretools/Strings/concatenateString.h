
#ifndef CONCATENATESTRING_H_
#define CONCATENATESTRING_H_

#include "coretools/Main/TError.h"
#include "coretools/Strings/toBuffer.h"

#include <string>
#include <vector>

namespace coretools::str {

template<typename Container>
std::string concatenateString(const Container &vec, std::string_view delim, size_t from = 0) {
	if (vec.empty() || vec.size() <= from) return std::string{};

	std::string s;
	s.reserve(delim.size() * (vec.size() + 1)); // at least

	auto b  = std::back_inserter(s);
	auto it = vec.cbegin() + from;
	toBuffer(b, *it);
	++it;
	for (; it != vec.cend(); ++it) { toBuffer(b, delim, *it); }
	return s;
};

template<typename T> std::string concatenateString(const T &vec) { return concatenateString(vec, ""); };

template<typename T> void fillConcatenatedString(const T &vec, std::string &s, std::string_view delim = "") {
	s = concatenateString(vec, delim);
};

template<typename T> std::string concatenateString(const T &vec, size_t from) {
	return concatenateString(vec, "", from);
};

template<typename T> void fillConcatenatedString(const T &vec, std::string &s, size_t from) {
	s = concatenateString(vec, "", from);
};

template<typename T> void fillConcatenatedString(const T &vec, std::string &s, std::string_view delim, size_t from) {
	s = concatenateString(vec, delim, from);
};

// function replicating the past behavior of R
template<typename T, typename U>
std::vector<std::string> paste(const T &first, const U &second, std::string_view delim) {
	if (first.size() == second.size()) {
		// equal length: simple pasting
		std::vector<std::string> ret(first.size());

		for (size_t i = 0; i < first.size(); ++i) { toBuffer(std::back_inserter(ret[i]), first[i], delim, second[i]); }

		return ret;
	} else if (first.size() % second.size() == 0) {
		// recycle second
		std::vector<std::string> ret(first.size());

		size_t j = 0;
		for (size_t i = 0; i < first.size(); ++i, ++j) {
			if (j == second.size()) { j = 0; }
			toBuffer(std::back_inserter(ret[i]), first[i], delim, second[j]);
		}

		return ret;
	} else if (second.size() % first.size() == 0) {
		// recycle first
		std::vector<std::string> ret(second.size());

		size_t j = 0;
		for (size_t i = 0; i < second.size(); ++i, ++j) {
			if (j == first.size()) { j = 0; }
			toBuffer(std::back_inserter(ret[i]), first[j], delim, second[i]);
		}

		return ret;
	} else {
		DEVERROR("sizes are not multiples oif each other!");
	}
};
}

#endif
