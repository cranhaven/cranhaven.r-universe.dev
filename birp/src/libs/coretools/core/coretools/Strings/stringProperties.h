
#ifndef STRINGPROPERTIES_H_
#define STRINGPROPERTIES_H_

#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

#include "coretools/Strings/fromString.h"

namespace coretools::str {

//-------------------------------------------------
// String contains
//-------------------------------------------------
template<typename T> constexpr bool stringContains(std::string_view haystack, T needle) {
	return haystack.find(needle) != std::string_view::npos;
}
constexpr bool stringContainsAny(std::string_view haystack, std::string_view needle) {
	return haystack.find_first_of(needle) != std::string::npos;
}

constexpr bool stringContainsOnly(std::string_view haystack, std::string_view needle) {
	return !haystack.empty() && (haystack.find_first_not_of(needle) == std::string::npos);
}
// numbers
constexpr bool stringContainsNumbers(std::string_view haystack) { return stringContainsAny(haystack, "1234567890"); }
constexpr bool stringContainsOnlyNumbers(std::string_view haystack) {
	return stringContainsOnly(haystack, "1234567890");
}
constexpr bool stringIsProbablyANumber(std::string_view haystack) {
	return stringContainsOnly(haystack, "1234567890.Ee-+");
}
constexpr bool stringIsProbablyABool(std::string_view haystack) {
	return haystack == "true" || haystack == "false" || haystack == "1" || haystack == "0";
}

// letters
constexpr bool stringContainsLetters(std::string_view haystack) {
	return stringContainsAny(haystack, "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVWXYZäöüÄÖÜàéèÀÉÈ");
}
constexpr bool stringContainsOnlyLetters(std::string_view haystack) {
	return stringContainsOnly(haystack, "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVWXYZäöüÄÖÜàéèÀÉÈ");
}

// starts with
constexpr bool stringStartsWith(std::string_view haystack, std::string_view needle) {
	return haystack.substr(0, needle.size()) == needle;
}
constexpr bool stringStartsWith(std::string_view haystack, char needle) {
	return !haystack.empty() && (haystack.front() == needle);
}

// test for uniqueness
template<typename Container> bool allEntriesAreUnique(Container vec) {
	std::sort(vec.begin(), vec.end());
	return std::adjacent_find(vec.begin(), vec.end()) == vec.end();
}

template<typename Container> auto getIteratorToFirstNonUniqueEntry(const Container &vec) {
	auto it_second = vec.end();
	for (auto it = vec.begin(); it != --vec.end(); ++it) {
		for (it_second = it + 1; it_second != vec.end(); ++it_second) {
			if (*it == *it_second) return it;
		}
	}
	return it_second;
};

/* *
 * Finding the nth Instance of a Substring
 * Stolen from: https://www.oreilly.com/library/view/c-cookbook/0596007612/ch04s11.html
 *
 * @tparam DelimType is the type of delimiter, e.g. std::string or char
 *
 * @param[in] N is the number of occurrences
 * @param[in] HayStack is the string to search in
 * @param[in] Needle is the pattern to search for
 *
 * @return position of Nth match. If it didn't match N times, std::string::npos is returned
 *
 * Attention: does not deal with overlapping matches (e.g. beginning of the string matches part of the end of the same
 * string, as in the word “abracadabra,” where the last four characters are the same as the first four)
 */
template<typename DelimType>
size_t findNthInstanceInString(size_t N, std::string_view HayStack, const DelimType &Needle) {
	auto i = HayStack.find(Needle); // find the first occurrence

	size_t j;
	for (j = 1; j < N && i != std::string_view::npos; ++j) {
		i = HayStack.find(Needle, i + 1); // find the next occurrence
	}

	if (j == N) {
		return i;
	} else {
		return std::string_view::npos;
	}
}

template<bool allowSubstitutions = true>
double levenshteinDistance(std::string_view s, std::string_view t, double matchingReward = 0.0) {
	// calculate Levenshtein Distance. Code taken from Wikipedia and adapted by:
	// - adding an option to disallow substitutions
	// - added option to reward exact matches
	size_t m = s.length();
	size_t n = t.length();

	// create two work vectors of integer distances
	std::vector<double> v0(n+1);
	std::vector<double> v1(n+1);

	// initialize v0 (the previous row of distances)
	// this row is A[0][i]: edit distance for an empty s
	// the distance is just the number of characters to delete from t
	for (unsigned int i = 0; i <= n; ++i) { 
		v0[i] = i;
	}

	for (unsigned int i = 0; i < m; ++i) {
		// calculate v1 (current row distances) from the previous row v0
		// first element of v1 is A[i+1][0]
		//   edit distance is delete (i+1) chars from s to match empty t
		v1[0] = i + 1;

		// use formula to fill in the rest of the row
		for (unsigned int j = 0; j < n; ++j) {
			// calculating costs for A[i+1][j+1]

			double min_del_or_ins = std::min(v0[j+1] + 1, v1[j] + 1);
			double sub = v0[j];

			if (s[i] != t[j]) {
				if constexpr(allowSubstitutions){
					sub += 1;				
				} else {
					sub += 2;				
				}
			} else {
				sub -= matchingReward;
			}		
			v1[j + 1] = std::min(min_del_or_ins, sub);
		}

		// copy v1 (current row) to v0 (previous row) for next iteration
		std::swap(v0, v1);
	}

	// after the last swap, the results of v1 are now in v0
	return v0[n];
};

template<bool allowSubstitutions = true, typename ContainerType>
std::pair<std::string, double> findClosestMatchLevenshtein(std::string_view needle, ContainerType haystack, double matchingReward = 0.0) {
	//comapre in lower case only
	std::string needle_lower = (std::string) needle;
	std::transform(needle_lower.begin(), needle_lower.end(), needle_lower.begin(), ::tolower);

	// initialize crazy values
	std::string_view bestMatch;
	double minDistance = 99999999;

	//compare to each element of haystack
	for(auto& h : haystack){
		//make lower case
		std::string haystack_lower = h;
		std::transform(haystack_lower.begin(), haystack_lower.end(), haystack_lower.begin(), ::tolower);
		double dist = str::levenshteinDistance<allowSubstitutions>(needle_lower, haystack_lower, matchingReward);
		if (dist < minDistance) {
			bestMatch   = h;
			minDistance = dist;
		}
	}

	return std::pair<std::string, double>((std::string) bestMatch, minDistance);
};

template<typename T> bool equalAs(std::string_view s1, std::string_view s2, const T &maxDiff) {
	T val1, val2;
	fromString<true>(s1, val1);
	fromString<true>(s2, val2);
	return !(std::fabs(val1 - val2) > maxDiff);
};

} // namespace coretools::str

#endif
