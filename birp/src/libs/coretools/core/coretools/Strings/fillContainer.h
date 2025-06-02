#ifndef FILLCONTAINER_H_
#define FILLCONTAINER_H_

#include <string>

#include "coretools/Strings/fromString.h"
#include "coretools/Strings/stringManipulations.h"

namespace coretools::str {

// convert whole containers, element by element
template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringContainer(const ContainerType<std::string> &input, ContainerType<StorageType> &Container) {
	Container.resize(input.size());
	for (size_t i = 0; i < input.size(); ++i) { fromString(Container[i], input[i]); }
};

template<template<typename...> class ContainerType>
void fillContainerFromStringContainer(const ContainerType<std::string> &input, ContainerType<std::string> &Container) {
	// template specialization to avoid expensive string to string conversion
	Container = input;
};

// convert whole containers, element by element
template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringContainerCheck(const ContainerType<std::string> &input,
                                           ContainerType<StorageType> &Container) {
	Container.resize(input.size());
	for (size_t i = 0; i < input.size(); ++i) { fromString<true>(Container[i], input[i]); }
};

template<template<typename...> class ContainerType>
void fillContainerFromStringContainerCheck(const ContainerType<std::string> &input,
                                           ContainerType<std::string> &Container) {
	// template specialization to avoid expensive string to string conversion
	Container = input;
};

//-------------------------------------------------
// Split into vector/set/list
//-------------------------------------------------

/* *
 * Split a string into a container
 *
 * @tparam ContainerType is the type of container where the elements should be stored in, e.g.
 * std::vector/std::set/std::list etc.
 * @tparam StorageType is the type that should be stored inside the container, e.g. std::string/double/int etc.
 *
 * @param[in] String is the string to split
 * @param[out] Container is the container to fill. Container can be std::vector, std::set, std::unordered_set, std::list
 * etc. or any other instance that has clear(), insert() and end() functions defined
 * @param[in] Delim is the delimiter used to split the string. It is given as a string (and not templated for char), in
 * order to be able to use the .size() function
 * @param[in] SkipEmpty is a bool indicating whether empty strings (e.g. if there are two consecutive delimiters) should
 * be skipped
 * @param[in] Check is a bool indicating whether string to StorageType conversion should be checked on validity
 * @param[in] Any is a bool indicating whether it is sufficient to match any character of Delim in order to split (Any =
 * true), or whether the entire sequence of Delim must match (Any=false)
 */

template<bool SkipEmpty = false, bool Check = false, bool Any = false, typename Type, template<typename...> typename Container>
void fillContainerFromString(std::string_view s, Container<Type>& cs, std::string_view delim) {
	cs.clear();
	TSplitter<std::string_view, Any> spl(s, delim);
	for (auto sp : spl) {
		auto s = strip(sp);
		if constexpr (SkipEmpty) if (s.empty()) continue;
		Type t{};
		fromString<Check>(s, t);
		cs.insert(cs.end(), t);
	}
}

template<bool SkipEmpty = false, bool Check = false, typename Type, template<typename...> typename Container>
void fillContainerFromStringAny(std::string_view s, Container<Type>& cs, std::string_view delim) {
	fillContainerFromString<SkipEmpty, Check, true>(s, cs, delim);
}

template<bool SkipEmpty = false, bool Check = false, typename Type, template<typename...> typename Container>
void fillContainerFromString(std::string_view s, Container<Type>& cs, char delim) {
	cs.clear();
	TSplitter spl(s, delim);
	for (auto sp : spl) {
		auto s = strip(sp);
		if constexpr (SkipEmpty) if (s.empty()) continue;
		Type t{};
		fromString<Check>(s, t);
		cs.insert(cs.end(), t);
	}
}

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromString(std::string_view String, ContainerType<StorageType> &Container, std::string_view delim,
                             bool SkipEmpty, bool Check=false, bool Any=false) {
	if (SkipEmpty) {
		if (Check) {
			if (Any) {
				fillContainerFromString<true, true, true>(String, Container, delim);
			}
			else {
				fillContainerFromString<true, true, false>(String, Container, delim);
			}
		} else /* not check */ {
			if (Any) {
				fillContainerFromString<true, false, true>(String, Container, delim);
			}
			else {
				fillContainerFromString<true, false, false>(String, Container, delim);
			}
		}
	} else /* not SkipEmpty */{
		if (Check) {
			if (Any) {
				fillContainerFromString<false, true, true>(String, Container, delim);
			}
			else {
				fillContainerFromString<false, true, false>(String, Container, delim);
			}
		} else /* not check */ {
			if (Any) {
				fillContainerFromString<false, false, true>(String, Container, delim);
			}
			else {
				fillContainerFromString<false, false, false>(String, Container, delim);
			}
		}
	}
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromString(std::string_view String, ContainerType<StorageType> &Container, char Delim,
                             bool SkipEmpty, bool Check = false) {
	if (SkipEmpty) {
		if (Check) {
			fillContainerFromString<true, true>(String, Container, Delim);
		} else /* not check */ {
			fillContainerFromString<true, false>(String, Container, Delim);
		}
	} else /* not SkipEmpty */{
		if (Check) {
			fillContainerFromString<false, true>(String, Container, Delim);
		} else /* not check */ {
			fillContainerFromString<false, false>(String, Container, Delim);
		}
	}
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringAny(std::string_view String, ContainerType<StorageType> &Container, std::string_view Delim,
                                bool SkipEmpty, bool Check = false) {
	// little helper function - if any=true, but otherwise default for SkipEmpty and Check, this function provides a
	// nicer interface

	if (SkipEmpty) {
		if (Check) {
			fillContainerFromStringAny<true, true>(String, Container, Delim);
		} else /* not check */ {
			fillContainerFromStringAny<true, false>(String, Container, Delim);
		}
	} else /* not SkipEmpty */ {
		if (Check) {
			fillContainerFromStringAny<false, true>(String, Container, Delim);
		} else /* not check */ {
			fillContainerFromStringAny<false, false>(String, Container, Delim);
		}
	}
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringWhiteSpace(std::string_view String, ContainerType<StorageType> &Container,
                                       bool SkipEmpty = false, bool Check = false) {
	fillContainerFromString(String, Container, " \t", SkipEmpty, Check, true);
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringRepeatIndexes(std::string_view String, ContainerType<StorageType> &Container,
                                          std::string_view Delim, bool SkipEmpty = false, bool Check = false,
                                          bool Any = false) {
	// same as above but repeats entries ( blah{x} turns into x times blah) and expands indexes ( blah[x] turn into
	// blah_1, blah_2, ..., blah_x) 1) split into vector of strings
	ContainerType<std::string> tmp;
	fillContainerFromString(String, tmp, Delim, SkipEmpty, false, Any);

	// 2) repeat and convert
	repeatIndexes(tmp, Container, Check);
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringRepeatIndexes(std::string_view String, ContainerType<StorageType> &Container, char Delim,
                                          bool SkipEmpty = false, bool Check = false) {
	// Same as above, but takes Delim as 'char'

	ContainerType<std::string> tmp;
	fillContainerFromString(String, tmp, Delim, SkipEmpty, false);

	// 2) repeat and convert
	repeatIndexes(tmp, Container, Check);
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringRepeatAndExpandIndexes(std::string_view String, ContainerType<StorageType> &Container,
                                                   std::string_view Delim, bool SkipEmpty = false, bool Check = false,
                                                   bool Any = false) {
	// same as above but repeats entries ( blah{x} turns into x times blah) and expands indexes ( blah[x] turn into
	// blah_1, blah_2, ..., blah_x) 1) split into vector of strings
	ContainerType<std::string> tmp;
	fillContainerFromString(String, tmp, Delim, SkipEmpty, false, Any);

	// 2) repeat and convert
	repeatAndExpandIndexes(tmp, Container, Check);
};

template<typename StorageType, template<typename...> class ContainerType>
void fillContainerFromStringRepeatAndExpandIndexes(std::string_view String, ContainerType<StorageType> &Container,
                                                   char Delim, bool SkipEmpty = false, bool Check = false) {
	// Same as above, but takes Delim as 'char'
	ContainerType<std::string> tmp;
	fillContainerFromString(String, tmp, Delim, SkipEmpty, false);

	// 2) repeat and convert
	repeatAndExpandIndexes(tmp, Container, Check);
};
}

#endif
