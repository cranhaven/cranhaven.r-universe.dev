#include "stringManipulations.h"
#include "coretools/Strings/splitters.h"

#include <algorithm>
#include <cctype>
namespace coretools::str {

//-----------------------------------------------------------------------
// Compare and replace
//-----------------------------------------------------------------------
std::string stringReplace(char Needle, std::string_view Replace, std::string_view Haystack) {
	std::string s;
	auto l = Haystack.find_first_of(Needle);
	while (l != std::string::npos) {
		s.append(Haystack.substr(0, l)).append(Replace);
		Haystack.remove_prefix(l + 1);
		l = Haystack.find_first_of(Needle);
	}

	s.append(Haystack);
	return s;
};

std::string stringReplace(std::string_view Needle, std::string_view Replace, std::string_view Haystack) {
	std::string s;
	std::string::size_type l = Haystack.find(Needle);
	while (l != std::string::npos) {
		s.append(Haystack.substr(0, l)).append(Replace);
		Haystack.remove_prefix(l + Needle.size());
		l = Haystack.find(Needle);
	}
	s.append(Haystack);
	return s;
};

//-----------------------------------------------------------------------
// Erase, extract and trim
//-----------------------------------------------------------------------
void eraseAllOccurences(std::string &s, std::string_view needle, bool any) {
	if (any) { // match any
		std::string::size_type l = s.find_first_of(needle);
		while (l != std::string::npos) {
			s.erase(l, 1);
			l = s.find_first_of(needle);
		}
	} else { // match exactly
		std::string::size_type l = s.find(needle);
		while (l != std::string::npos) {
			s.erase(l, needle.size());
			l = s.find(needle);
		}
	}
}

void eraseAllOccurences(std::string &s, char needle) {
	std::string needleString;
	needleString = needle;
	eraseAllOccurences(s, needleString);
};

void eraseAllWhiteSpaces(std::string &s) { eraseAllOccurences(s, " \t\f\v\n\r", true); };

void eraseAllNonAlphabeticCharacters(std::string &s) {
	s.erase(remove_if(s.begin(), s.end(), [](char c) { return !isalpha(c); }), s.end());
}

// read

/////////////////// read before ///////////////////

std::string_view readBefore(std::string_view s, std::string_view needle, bool any) {
	const auto p = any ? s.find_first_of(needle) : s.find(needle);
	return s.substr(0, p);
}

std::string_view readBefore(std::string_view s, char needle) {
	const auto p = s.find(needle);
	return s.substr(0, p);
}

/////////////////// read until ///////////////////

std::string_view readUntil(std::string_view s, std::string_view needle, bool any) {
	const auto p = any ? s.find_first_of(needle) : s.find(needle);
	if (p == std::string_view::npos) return s;

	const auto size = any ? 1 : needle.size();
	return s.substr(0, p + size);
}

std::string_view readUntil(std::string_view s, char needle) {
	const auto p = s.find(needle);

	if (p == std::string_view::npos) return s;
	return s.substr(0, p + 1);
}

/////////////////// read before last ///////////////////

std::string_view readBeforeLast(std::string_view s, std::string_view needle, bool any) {
	const auto p = any ? s.find_last_of(needle) : s.rfind(needle);
	return s.substr(0, p);
};

std::string_view readBeforeLast(std::string_view s, char needle) {
	const auto p = s.rfind(needle);
	return s.substr(0, p);
};

/////////////////// read until last ///////////////////

std::string_view readUntilLast(std::string_view s, std::string_view needle, bool any) {
	const auto p = any ? s.find_last_of(needle) : s.rfind(needle);
	if (p == std::string_view::npos) return s;

	const auto size = any ? 1 : needle.size();
	return s.substr(0, p + size);
};

std::string_view readUntilLast(std::string_view s, char needle) {
	const auto p = s.rfind(needle);

	if (p == std::string_view::npos) return s;
	return s.substr(0, p + 1);
};

/////////////////// read after ///////////////////

std::string_view readAfter(std::string_view s, std::string_view needle, bool any) {
	const auto p = any ? s.find_first_of(needle) : s.find(needle);
	if (p == std::string_view::npos) return std::string_view{};

	const auto size = any ? 1 : needle.size();
	return s.substr(p + size);
};

std::string_view readAfter(std::string_view s, char needle) {
	const auto p = s.find(needle);
	if (p == std::string_view::npos) return std::string_view{};

	return s.substr(p + 1);
};

/////////////////// read after last ///////////////////

std::string_view readAfterLast(std::string_view s, std::string_view needle, bool any) {
	const auto p = any ? s.find_last_of(needle) : s.rfind(needle);
	if (p == std::string_view::npos) return std::string_view{};

	const auto size = any ? 1 : needle.size();
	return s.substr(p + size);
};

std::string_view readAfterLast(std::string_view s, char needle) {
	const auto p = s.rfind(needle);
	if (p == std::string_view::npos) return std::string_view{};

	return s.substr(p + 1);
};

/////////////////// read between ///////////////////

std::string_view readBetween(std::string_view s, std::string_view left, std::string_view right, bool any) {
	const auto l = any ? s.find_first_of(left) : s.find(left);
	if (l == std::string_view::npos) return std::string_view{};

	const auto size = any ? 1 : left.size();
	const auto r    = any ? s.find_first_of(right, l + size) : s.find(right, l + size);
	return s.substr(l + size, r);
}

std::string_view readBetween(std::string_view s, char left, char right) {
	const auto l = s.find(left);
	if (l == std::string_view::npos) return std::string_view{};

	const auto r = s.find(right, l + 1);
	return s.substr(l + 1, r);
}

/////////////////// extract before ///////////////////
std::string extractBefore(std::string &s, std::string_view needle, bool any) {
	// will return everything before (and not including) the first match of needle
	// will modify s to contain everything after (including) needle
	// if any: will match any character in needle; else: will match needle exactly
	std::string ret;
	std::string::size_type l;
	if (any)
		l = s.find_first_of(needle);
	else
		l = s.find(needle);
	if (l != std::string::npos) {
		ret = s.substr(0, l);
		s.erase(0, l);
	} else {
		ret = s;
		s.clear();
	}
	return ret;
}

std::string extractBefore(std::string &s, char needle) {
	std::string needleString;
	needleString = needle;
	return extractBefore(s, needleString);
};

std::string extractBeforeDoubleSlash(std::string &s) {
	// will return everything before (and not including) //
	// will modify s to contain everything after (and including) //
	return extractBefore(s, "//", false);
};

std::string extractBeforeWhiteSpace(std::string &s) {
	// will return everything before (and not including) the first whitespace
	// will modify s to contain everything after (including) the first whitespace
	return extractBefore(s, " \t\f\v\n\r", true);
};

/////////////////// extract until ///////////////////

std::string extractUntil(std::string &s, std::string_view needle, bool any) {
	// will return everything before (and including) the first match of needle
	// will modify s to contain everything after (and not including) the first match of needle
	// if any: will match any character in needle; else: will match needle exactly
	int size = 1;
	if (!any) size = needle.size();
	std::string ret;
	std::string::size_type l;

	if (any)
		l = s.find_first_of(needle);
	else
		l = s.find(needle);
	if (l != std::string::npos) {
		ret = s.substr(0, l + size);
		s.erase(0, l + size);
	} else {
		ret = s;
		s.clear();
	}
	return ret;
}

std::string extractUntil(std::string &s, char needle) {
	std::string needleString;
	needleString = needle;
	return extractUntil(s, needleString);
};

/////////////////// extract before last ///////////////////

std::string extractBeforeLast(std::string &s, std::string_view needle, bool any) {
	// will return everything before (and not including) the last match of needle
	// will modify s to contain everything after (and including) the last match of needle
	// if any: will match any character in needle; else: will match needle exactly
	std::string ret;
	std::string::size_type l;
	if (any)
		l = s.find_last_of(needle);
	else
		l = s.rfind(needle);
	if (l != std::string::npos) {
		ret = s.substr(0, l);
		s.erase(0, l);
	} else {
		ret = s;
		s.clear();
	}
	return ret;
}

std::string extractBeforeLast(std::string &s, char needle) {
	std::string needleString;
	needleString = needle;
	return extractBeforeLast(s, needleString);
};

/////////////////// extract until last ///////////////////

std::string extractUntilLast(std::string &s, std::string_view needle, bool any) {
	// will return everything before (and including) the last match of needle
	// will modify s to contain everything after (and not including) the last match of needle
	// if any: will match any character in needle; else: will match needle exactly
	int size = 1;
	if (!any) size = needle.size();
	std::string ret;
	std::string::size_type l;

	if (any)
		l = s.find_last_of(needle);
	else
		l = s.rfind(needle);
	if (l != std::string::npos) {
		ret = s.substr(0, l + size);
		s.erase(0, l + size);
	} else {
		ret = s;
		s.clear();
	}
	return ret;
}

std::string extractUntilLast(std::string &s, char needle) {
	std::string needleString;
	needleString = needle;
	return extractUntilLast(s, needleString);
};

std::string extractPath(std::string &s) {
	// will return everything before (and including) the last /
	// will modify s to contain everything after (and not including) the last /
	return extractUntilLast(s, "/");
};

/////////////////// extract after ///////////////////

std::string extractAfter(std::string &s, std::string_view needle, bool any) {
	// will return everything after (while not including) the first match of needle
	// will modify s to contain everything before (and including) the first match of needle
	// if any: will match any character in needle; else: will match needle exactly
	int size = 1;
	if (!any) size = needle.size();
	std::string ret;
	std::string::size_type l;

	if (any)
		l = s.find_first_of(needle);
	else
		l = s.find(needle);
	if (l != std::string::npos) {
		ret = s.substr(l + size);
		s.erase(l + size);
	} else {
		ret = s;
		s.clear();
	}
	return ret;
}

std::string extractAfter(std::string &s, char needle) {
	std::string needleString;
	needleString = needle;
	return extractAfter(s, needleString);
};

/////////////////// extract after last ///////////////////

std::string extractAfterLast(std::string &s, std::string_view needle, bool any) {
	// will return everything after (while not including) the last match of needle
	// will modify s to contain everything before (and including) the last match of needle
	// if any: will match any character in needle; else: will match needle exactly
	int size = 1;
	if (!any) size = needle.size();
	std::string ret;
	std::string::size_type l;

	if (any)
		l = s.find_last_of(needle);
	else
		l = s.rfind(needle);
	if (l != std::string::npos) {
		ret = s.substr(l + size);
		s.erase(l + size);
	} else {
		ret = s;
		s.clear();
	}
	return ret;
}

std::string extractAfterLast(std::string &s, char needle) {
	return extractAfterLast(s, std::string(1, needle), false);
};

/////////////////// extract between ///////////////////

std::string extractBetween(std::string &s, std::string_view left, std::string_view right, bool any){
	// will return everything between (while not including) the firts match of left and the next match of right
	// will modify s to contain everything before (and including) left and everything after (and including) right.
	// if any: will match any character in needle; else: will match needle exactly
	int size_l = 1;
	int size_r = 1;
	if (!any){
		size_l = left.size();
		size_r = left.size();
	}
	std::string ret;
	std::string::size_type l,r;

	if (any){
		l = s.find_first_of(left);
		r = s.find_first_of(right, l);
	} else {
		l = s.find(left);
		r = s.find(right, l + size_l);
	}
	if (l == std::string::npos || r == std::string::npos) {
		//return empty string
		ret = s;
		s.clear();
	} else {
		//found left and right: extract!
		ret = s.substr(l + size_l) + s.substr(r + size_r, std::string::npos);
		s.erase(r, std::string::npos);
		s.erase(0, l + size_l);
	}
	return ret;
}

std::string extractBetween(std::string &s, char left, char right){
	return extractBetween(s, std::string(1, left), std::string(1, right), false);
}

/////////////////// trim ///////////////////
void trimString(std::string &s, std::string_view what) {
	// will trim all occurrences of what in the beginning and in the end of s
	// will match any character of what (what does not have to match entirely)
	// from beginning
	std::string::size_type l = s.find_first_not_of(what);
	if (l == std::string::npos) {
		s.clear();
	} else {
		s.erase(0, l);
		// from end
		l = s.find_last_not_of(what);
		if (l != std::string::npos) s.erase(l + 1);
	}
};

void trimEndlineString(std::string &s) {
	// will trim all trailing and ending endline characters of s.
	// endline characters in the middle of s will be kept (i.e. as soon as a non-endline character occurs)
	trimString(s, "\f\v\n\r");
};

std::string getTrimmedString(std::string_view s) {
	std::string tmp{s};
	trimString(tmp);
	return tmp;
};

std::string getTrimmedString(std::string &s, std::string_view what) {
	std::string tmp = s;
	trimString(tmp, what);
	return tmp;
};

std::string getTrimmedEndlineString(std::string &s) {
	std::string tmp = s;
	trimEndlineString(tmp);
	return tmp;
};

/////////////////// split ///////////////////

std::string splitAny(std::string &s, std::string_view delim) {
	std::string before;
	std::string::size_type l = s.find_first_of(delim);
	if (l != std::string::npos) {
		before = s.substr(0, l);
		s.erase(0, l + 1);
	} else {
		before = s;
		s.clear();
	}
	return before;
}

std::string splitExactly(std::string &s, std::string_view delim) {
	std::string before;
	std::string::size_type l = s.find(delim);
	if (l != std::string::npos) {
		before = s.substr(0, l);
		s.erase(0, l + delim.size());
	} else {
		before = s;
		s.clear();
	}
	return before;
}

std::string split(std::string &s, std::string_view delim, bool any) {
	if (any) return splitAny(s, delim);
	return splitExactly(s, delim);
}

std::string split(std::string &s, char delim) {
	std::string delimString;
	delimString = delim;
	return split(s, delimString);
}

/////////////////// split at last ///////////////////

std::string splitLastAny(std::string &s, std::string_view delim) {
	std::string before;
	std::string::size_type l = s.find_last_of(delim);
	if (l != std::string::npos) {
		before = s.substr(0, l);
		s.erase(0, l + 1);
	} else {
		before = s;
		s.clear();
	}
	return before;
}

std::string splitLastExactly(std::string &s, std::string_view delim) {
	std::string before;
	std::string::size_type l = s.rfind(delim);
	if (l != std::string::npos) {
		before = s.substr(0, l);
		s.erase(0, l + delim.size());
	} else {
		before = s;
		s.clear();
	}
	return before;
}

std::string splitAtLast(std::string &s, std::string_view delim, bool any) {
	if (any) return splitLastAny(s, delim);
	return splitLastExactly(s, delim);
}

std::string splitAtLast(std::string &s, char delim) {
	std::string delimString;
	delimString = delim;
	return splitAtLast(s, delimString);
}

/////////////////// split at position ///////////////////

std::string splitAtPos(std::string &s, std::string::size_type l, size_t Length) {
	std::string before;
	if (l != std::string::npos) {
		before = s.substr(0, l);
		s.erase(0, l + Length);
	} else {
		before = s;
		s.clear();
	}
	return before;
}


/////////////////// capitalize ///////////////////
std::string capitalizeFirst(std::string s){
    if (s.length() == 0) {
        return s;
    }

    s.front() = std::toupper(s.front());
    return s;
}

std::string camelCase(std::string_view Phrase) {
	std::string camel;
	TWhitespaceSplitter spl(Phrase);
	for (auto sp: spl) {
		size_t p = camel.size();
		camel.append(sp);
		camel[p] = std::toupper(camel[p]);
	}
	return camel;
}

std::string snake_case(std::string_view Phrase) {
	std::string snake;
	TWhitespaceSplitter spl(Phrase);
	for (auto sp: spl) {
		snake.append(sp);
		snake.append(1, '_');
	}
	snake.pop_back();
	return snake;
}
}
