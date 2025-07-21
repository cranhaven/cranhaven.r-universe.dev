/*
 * TGzReader.h
 *
 *  Created on: 2023-08-07
 *      Author: Andreas
 */

#ifndef CORE_FILES_TGZREADER_H
#define CORE_FILES_TGZREADER_H

#include <string_view>
#include <zlib.h>

#include "coretools/Files/TReader.h"
#include "coretools/Main/TError.h"


namespace coretools {

class TGzReader final : public TReader {
	gzFile _file;

	size_t _read(void *buffer, size_t size, size_t count) override {
		const int n = gzread(_file, buffer, size * count);
		if (n < 0) { throw TDevError("Was not able to read file ", name(), "!"); }
		return n / size;
	}
	int64_t _tell() const override { return gztell(_file); }
	void _seek(int64_t pos) override {
		gzclearerr(_file);
		if (gzseek(_file, pos, SEEK_SET) == -1) {
			throw TDevError("Was not able to reach position ", pos, " in file ", name(), "!");
		}
	}
	bool _eof() const override { return gzeof(_file); }

public:
	TGzReader(std::string_view Filename) : TReader(Filename), _file(gzopen(name().c_str(), "r")) {
		user_assert(_file, "Was not able to open file ", name(), ". Does the file exist?");
	}
	~TGzReader() { gzclose(_file); }
	// Copying pointers is a problem
	TGzReader(const TGzReader &)            = delete;
	TGzReader &operator=(const TGzReader &) = delete;

	// Only allow move
	TGzReader(TGzReader &&)            = default;
	TGzReader &operator=(TGzReader &&) = default;
};

} // namespace coretools

#endif
