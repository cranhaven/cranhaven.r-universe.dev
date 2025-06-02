/*
 * TStdReader.h
 *
 *  Created on: 2023-08-07
 *      Author: Andreas
 */

#ifndef CORE_FILES_TSTDREADER_H
#define CORE_FILES_TSTDREADER_H

#include <cstdio>
#include <string_view>

#include "coretools/Files/TReader.h"
#include "coretools/Main/TError.h"

namespace coretools {

class TStdReader final : public TReader {
	std::FILE *_file;
	size_t _read(void *buffer, size_t size, size_t count) override {
		const size_t n = std::fread(buffer, size, count, _file);
		if (n == 0 && ferror(_file)) { DEVERROR("Was not able to read file ", name(), "!"); }
		return n;
	}
	int64_t _tell() const override { return std::ftell(_file); }
	void _seek(int64_t pos) override {
		std::clearerr(_file);
		if (std::fseek(_file, pos, SEEK_SET) != 0) {
			DEVERROR("Was not able to reach position ", pos, " in file ", name(), "!");
		}
	}
	bool _eof() const override { return std::feof(_file); }

public:
	TStdReader(std::string_view Filename) : TReader(Filename), _file(std::fopen(name().c_str(), "r")) {
		if (!_file) { UERROR("Was not able to open file ", name(), ". Does the file exist?"); }
	}
	~TStdReader() { std::fclose(_file); }
	// Copying pointers is a problem
	TStdReader(const TStdReader &)            = delete;
	TStdReader &operator=(const TStdReader &) = delete;

	// Only allow move
	TStdReader(TStdReader &&)            = default;
	TStdReader &operator=(TStdReader &&) = default;
};

} // namespace coretools

#endif
