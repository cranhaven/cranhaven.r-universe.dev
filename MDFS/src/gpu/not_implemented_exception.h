#ifndef NOT_IMPLEMENTED_EXCEPTION_H
#define NOT_IMPLEMENTED_EXCEPTION_H

#include <string>

struct NotImplementedException {
	const std::string msg;
	NotImplementedException(const std::string&& msg) : msg(msg) {}
};

#endif
