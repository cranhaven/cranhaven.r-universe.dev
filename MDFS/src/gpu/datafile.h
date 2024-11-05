#ifndef DATAFILE_CUH
#define DATAFILE_CUH

#include <stdint.h>

enum BinaryFormat { BF_SHIFT, BF_SPLIT };

struct InputFile {
	int vars;
	int objs;
	double* data;
	int* decision;
};

struct FileInfo {
	uint64_t nvar;
	uint64_t ndis;
	uint64_t ndiv;
	uint64_t ncls;
	uint64_t nbits;
	uint64_t psize;
	uint64_t nobjc0;
	uint64_t tobjc0;
	uint64_t nobjc1;
	uint64_t tobjc1;
	uint64_t varlen0;
	uint64_t varlen1;
	uint64_t disclen;
	std::size_t varsize;
	BinaryFormat binFormat;
};

struct DataFile {
	FileInfo fi;
	uint64_t* data;
	uint64_t* counters;
};

#endif
