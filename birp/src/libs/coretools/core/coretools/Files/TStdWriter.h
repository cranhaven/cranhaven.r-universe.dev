
/*
 * TWriter.h
 *
 *  Created on: 2023-07-05
 *      Author: Andreas
 */

#ifndef CORE_FILES_TSTDWRITER_H
#define CORE_FILES_TSTDWRITER_H

#include <cstdio>

#include "coretools/Files/TWriter.h"
#include "coretools/Main/TError.h"

namespace coretools {

class TStdWriter final : public TWriter {
	std::FILE *_file;

	void _write(const void *buffer, size_t size, size_t count) override {
		if (std::fwrite(buffer, size, count, _file) == 0) { throw TDevError("Was not able to write to file", name(), "!"); }
	};
	int64_t _tell() const override { return std::ftell(_file); };

public:
	TStdWriter(std::string_view Filename, const char *Mode = "w")
		: TWriter(Filename), _file(std::fopen(name().c_str(), Mode)) {
		user_assert(_file, "Was not able to create file ", name(), ". Does the path exist?");
	}
	~TStdWriter() { std::fclose(_file); }

	// Copying pointers is a problem
	TStdWriter(const TStdWriter &)            = delete;
	TStdWriter &operator=(const TStdWriter &) = delete;

	// Only allow move
	TStdWriter(TStdWriter &&)            = default;
	TStdWriter &operator=(TStdWriter &&) = default;
};
} // namespace coretools

#endif
