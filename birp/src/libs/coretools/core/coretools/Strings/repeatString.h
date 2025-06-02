#ifndef REPEATSTRING_H_
#define REPEATSTRING_H_

#include "coretools/Strings/fromString.h"
#include "coretools/Strings/toBuffer.h"

namespace coretools::str {


template<typename T> std::string repeat(const T &Input, size_t N, char Delim = ',') {
	if (N == 0) { return ""; }

	std::string s;
	auto b = std::back_inserter(s);
	toBuffer(b, Input);

	for (size_t i = 1; i < N; i++) { toBuffer(b, Delim, Input); }
	return s;
}

// expand / repeat index
bool addRepeatedIndexIfRepeated(std::string_view orig, std::vector<std::string> &vec);
bool addExpandedIndexIfToExpand(std::string_view orig, std::vector<std::string> &vec);
void addRepeatedIndex(std::string_view orig, std::vector<std::string> &vec);
void addExpandedIndex(std::vector<std::string> &Vec, std::string_view Prefix, int Length,
					  std::string_view Postfix = "");
void addExpandIndex(std::string_view orig, std::vector<std::string> &vec);
void addRepeatedAndExpandIndexes(std::string_view orig, std::vector<std::string> &vec);
void repeatAndExpandIndexes(const std::vector<std::string> &orig, std::vector<std::string> &vec);

template<typename StorageType, template<typename...> class ContainerType>
void repeatIndexes(ContainerType<std::string> &orig, ContainerType<StorageType> &vec, bool Check = false) {
	vec.clear();
	ContainerType<std::string> tmp;
	for (auto it = orig.begin(); it != orig.end(); ++it) { addRepeatedIndex(*it, tmp); }

	for (auto it = tmp.begin(); it != tmp.end(); ++it) {
		if (Check) {
			vec.push_back(fromString<StorageType, true>(*it));
		} else {
			vec.push_back(fromString<StorageType>(*it));
		}
	}
};

template<typename StorageType, template<typename...> class ContainerType>
void repeatAndExpandIndexes(ContainerType<std::string> &orig, ContainerType<StorageType> &vec, bool Check = false) {
	vec.clear();
	ContainerType<std::string> tmp;
	for (auto it = orig.begin(); it != orig.end(); ++it) { addRepeatedAndExpandIndexes(*it, tmp); }

	for (auto it = tmp.begin(); it != tmp.end(); ++it) {
		if (Check) {
			vec.push_back(fromString<StorageType, true>(*it));
		} else {
			vec.push_back(fromString<StorageType>(*it));
		}
	}
};

void addRepeatedAndExpandedIndexesOfSub(std::string_view orig, std::vector<std::vector<std::string>> &vec,
										std::string_view delim);
void repeatAndExpandIndexesOfSubs(const std::vector<std::string> &orig, std::vector<std::vector<std::string>> &vec,
								  std::string_view delim);

template<typename T> std::vector<std::string> expandOverMultiDimensions(std::vector<T> vec) {
	// example: you have vector {3, 2, 2} (of some size, in this case 3) and would like get all combinations of values
	// up to the value in the vector in this case: 0_0_0 0_0_1 0_1_0 0_1_1 1_0_0 1_0_1 1_1_0 1_1_1 2_0_0 2_0_1 2_1_0
	// 2_1_1
	// this function solves this with a recursion; and return a vector of strings containing these combinations
	std::vector<std::string> result;
	std::vector<std::string> remaining;
	if (vec.size() > 1) {
		std::vector<T> vecWithoutFirstElement(vec.begin() + 1, vec.end());
		remaining = expandOverMultiDimensions<T>(vecWithoutFirstElement);
	}
	for (int i = 0; i < vec.at(0); i++) {
		if (vec.size() == 1)
			result.push_back(toString(i));
		else {
			for (auto &val : remaining) { result.push_back(toString(i) + "_" + val); }
		}
	}
	return result;
};

}; // namespace coretools::str

#endif /* STRINGFUNCTIONS_H_ */
