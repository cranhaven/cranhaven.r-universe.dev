
#include "coretools/Strings/repeatString.h"
#include "coretools/Strings/fillContainer.h"
#include "coretools/Strings/toBuffer.h"

namespace coretools::str {

// replace entry blah{x} with x times a blah entry
bool addRepeatedIndexIfRepeated(std::string_view orig, std::vector<std::string> &vec) {
	std::string::size_type pos = orig.find_last_of('{');
	if (pos != std::string::npos && pos != 0) {
		if (orig.find_first_of('{') != pos) throw TUserError("Multiple '{' characters in string to repeat '", orig, "'!");
		if (orig.find_last_of('}') != orig.size() - 1) throw TUserError("String to repeat '", orig, "' does not end with '}'!");
		if (orig.find_first_of('[') != std::string::npos)
			throw TUserError("String to repeat '", orig, "' contains a conflicting '[' character!");
		if (orig.find_first_of(']') != std::string::npos)
			throw TUserError("String to repeat '", orig, "' contains a conflicting ']' character!");
		auto tmp = orig.substr(0, pos);
		int len         = fromString<int, true>(orig.substr(pos + 1, orig.size() - pos - 2));
		if (len <= 0) throw TUserError("Request to repeat string '", orig, "' zero times!");
		for (int i = 1; i <= len; ++i) vec.emplace_back(tmp);
		return true;
	} else
		return false;
}

// add entries Prefix1Postfix, Prefix2Postfix, ..., PrefixLengthPostfix
void addExpandedIndex(std::vector<std::string> &Vec, std::string_view Prefix, int Length,
                      std::string_view Postfix) {
	Vec.reserve(Vec.size() + Length);
	for (int i = 1; i <= Length; ++i) {
		std::string s;
		toBuffer(std::back_inserter(s), Prefix, i, Postfix);
		Vec.push_back(s);
	}
};

// replace entry blah[x] with blah_1, ..., blah_x
bool addExpandedIndexIfToExpand(std::string_view orig, std::vector<std::string> &vec) {
	std::string::size_type pos = orig.find_last_of('[');
	if (pos != std::string::npos) {
		if (orig.find_first_of('[') != pos) throw TUserError("Multiple '[' characters in string to expand '", orig, "'!");
		std::string::size_type pos2 = orig.find_last_of(']');
		if (pos2 == std::string::npos) throw TUserError("Missing closing ']' in string to expand '", orig, "'!");
		if (orig.find_first_of(']') != pos2) throw TUserError("Multiple ']' characters in string to expand '", orig, "'!");
		if (pos2 < pos) throw TUserError("Unable to understand string to expand '", orig, "': wrong order of '[' and ']'!");
		int len = fromString<int>(orig.substr(pos + 1, pos2 - pos));
		if (len <= 0) throw TUserError("Request to expand string '", orig, "' zero times!");
		addExpandedIndex(vec, orig.substr(0, pos), len, orig.substr(pos2 + 1));
		return true;
	} else
		return false;
}

void addRepeatedIndex(std::string_view orig, std::vector<std::string> &vec) {
	if (!addRepeatedIndexIfRepeated(orig, vec)) vec.emplace_back(orig);
}

void addExpandIndex(std::string_view orig, std::vector<std::string> &vec) {
	if (!addExpandedIndexIfToExpand(orig, vec)) vec.emplace_back(orig);
};

void addRepeatedAndExpandIndexes(std::string_view orig, std::vector<std::string> &vec) {
	if (!addRepeatedIndexIfRepeated(orig, vec)) {
		if (!addExpandedIndexIfToExpand(orig, vec)) vec.emplace_back(orig);
	}
};

void repeatAndExpandIndexes(const std::vector<std::string> &orig, std::vector<std::string> &vec) {
	vec.clear();
	for (auto &it : orig) { addRepeatedAndExpandIndexes(it, vec); }
};

void addRepeatedAndExpandedIndexesOfSub(std::string_view orig, std::vector<std::vector<std::string>> &vec,
                                        std::string_view delim) {
	std::vector<std::string> origVec;
	fillContainerFromStringAny<true>(orig, origVec, delim);
	auto *tmpVec       = new std::vector<std::string>[orig.size()];
	unsigned int times = 1;

	// expand individually
	unsigned int i = 0;
	for (auto it = origVec.begin(); it != origVec.end(); ++it, ++i) {
		addRepeatedAndExpandIndexes(*it, tmpVec[i]);
		if (tmpVec[i].size() > 1) {
			if (times > 1) {
				if (tmpVec[i].size() != times) throw TUserError("Unequal number of expansions / repeats in '", orig, "'!");
			} else
				times = tmpVec[i].size();
		}
	}

	// construct new vectors
	for (i = 0; i < times; ++i) vec.emplace_back();
	for (i = 0; i < origVec.size(); ++i) {
		if (tmpVec[i].size() == 1) {
			auto it = vec.rbegin();
			for (unsigned int j = 0; j < times; ++j, ++it) it->push_back(*(tmpVec[i].begin()));
		} else {
			auto it = vec.rbegin();
			for (auto sIt = tmpVec[i].rbegin(); sIt != tmpVec[i].rend(); ++sIt, ++it) { it->push_back(*sIt); }
		}
	}
	delete[] tmpVec;
}

void repeatAndExpandIndexesOfSubs(const std::vector<std::string> &orig, std::vector<std::vector<std::string>> &vec,
                                  std::string_view delim) {
	vec.clear();
	for (auto &it : orig) { addRepeatedAndExpandedIndexesOfSub(it, vec, delim); }
}

} // namespace coretools::str
