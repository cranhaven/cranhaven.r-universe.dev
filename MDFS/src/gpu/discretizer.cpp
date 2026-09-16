#include <cmath>
#include "discretizer.h"
#include "allocator.h"
#include "discretize.hpp"

// std::memset
#include <cstring>

Discretizer::Discretizer(LaunchConfig lc,
	InputFile inf,
	Scheduler *scheduler) : lc(lc), inf(inf),
	scheduler(scheduler),
	buffer((int8_t*) MALLOC_HOST(inf.objs * sizeof(int8_t))) {

	int c0 = 0, c1 = 0;
	for (int i = 0; i < inf.objs; i++) {
		if (inf.decision[i] == 0) c0++;
		if (inf.decision[i] == 1) c1++;
	}

	df.fi.nvar = ((inf.vars + lc.tileSize - 1) / lc.tileSize) * lc.tileSize;
	df.fi.ndis = lc.disc;
	df.fi.ndiv = lc.div;
	df.fi.ncls = lc.div + 1;
	df.fi.nbits = std::ceil(std::log2((double) df.fi.ncls));
	if (lc.bf == BF_SPLIT && df.fi.nbits == 3) df.fi.nbits = 4;

	df.fi.psize = 64 / df.fi.nbits;

	int padding = 16 * df.fi.psize;

	df.fi.nobjc0 = ((c0 + padding - 1) / padding) * padding;
	df.fi.tobjc0 = c0;
	df.fi.nobjc1 = ((c1 + padding - 1) / padding) * padding;
	df.fi.tobjc1 = c1;
	df.fi.varlen0 = df.fi.nobjc0 / df.fi.psize;
	df.fi.varlen1 = df.fi.nobjc1 / df.fi.psize;
	df.fi.varsize = (df.fi.varlen0 + df.fi.varlen1) * sizeof(uint64_t);

	df.fi.disclen = df.fi.nvar * (df.fi.varlen0 + df.fi.varlen1);
	df.fi.binFormat = lc.bf;

	df.data = (uint64_t*) MALLOC_HOST(df.fi.ndis * df.fi.nvar * df.fi.varsize);

	std::memset(df.data, 0, df.fi.ndis * df.fi.nvar * df.fi.varsize);

	df.counters = (uint64_t*) MALLOC_PINNED(
		df.fi.ndis * df.fi.nvar * df.fi.ncls * sizeof(uint64_t));

	std::memset(df.counters, 0, df.fi.ndis * df.fi.nvar * df.fi.ncls * sizeof(uint64_t));
}

Discretizer::~Discretizer() {
	FREE_HOST(buffer);
	FREE_HOST(df.data);
	FREE_PINNED(df.counters);
}

void Discretizer::discretizeTile(int offset) {
	for (int var = offset; var < offset + lc.tileSize && var < inf.vars; var++) {
		for (int d = 0; d < lc.disc; d++) {
			discretize(lc.seed,
				d,
				var,
				lc.div,
				inf.objs,
				inf.data + (var * inf.objs),
				buffer,
				lc.range);

			if (lc.dim == 2) count(d, var);

			if (df.fi.binFormat == BF_SHIFT) {
				shiftCopy(d, var);
			}
			if (df.fi.binFormat == BF_SPLIT) {
				splitCopy(d, var);
			}
		}
	}
}

void Discretizer::count(int d, int var) {
	uint64_t* counters = df.counters + ((d * df.fi.nvar + var) * df.fi.ncls);

	for (int i = 0; i < inf.objs; i++) {
		counters[buffer[i]] += inf.decision[i] ? 1 : 1ll << 32;
	}
}

// nbits - bits per object (to encode its variable [feature] value)
// psize = 64/nbits (shift-only: how many fit in one uint64)

// shiftCopy - compress and copy data for shift (regular) kernels
//
// Compression fits `psize` objects in one uint64.
// A single object is in exactly one uint64, taking `nbits` space,
// shifted left from previous object.
// In case of `nbits=3`, one bit is lost (unused).
//
// d - discretization ID
// var - variable (feature) ID
void Discretizer::shiftCopy(int d, int var) {
	uint64_t* data0 = df.data + (d * df.fi.disclen + var * (df.fi.varlen0 + df.fi.varlen1));
	// num of objects copied already (per decision)
	int last[2] = {0, 0};
	// target mem addresses of packs (per decision)
	uint64_t* data[2] = {data0, data0 + df.fi.varlen0};

	for (int i = 0; i < inf.objs; i++) {
		int dec = inf.decision[i];

		data[dec][last[dec] / df.fi.psize] |=
			uint64_t(buffer[i]) << (df.fi.nbits * (last[dec] % df.fi.psize));

		last[dec]++;
	}
}

// splitCopy - compress and copy data for split&tables (bitvec) kernels
//
// Compression fits one bit for 64 objects in one uint64.
// A single object is split over `nbits` consecutive uint64s.
//
// d - discretization ID
// var - variable (feature) ID
void Discretizer::splitCopy(int d, int var) {
	uint64_t* data0 = df.data + (d * df.fi.disclen + var * (df.fi.varlen0 + df.fi.varlen1));
	// num of objects copied already (per decision)
	int last[2] = {0, 0};
	// target mem addresses of packs (per decision)
	uint64_t* data[2] = {data0, data0 + df.fi.varlen0};

	for (int i = 0; i < inf.objs; i++) {
		int dec = inf.decision[i];

		for (int j = 0; j < df.fi.nbits; j++) {
			uint64_t bit_j = (buffer[i] >> j) & 1;
			data[dec][df.fi.nbits * (last[dec] / (df.fi.psize * df.fi.nbits)) + j] |=
				bit_j << (last[dec] % (df.fi.psize * df.fi.nbits));
		}
		last[dec]++;
	}
}

DataFile Discretizer::getDataFile() {
	return df;
}

void Discretizer::workLoop() {
	for (int offset = 0; offset < inf.vars; offset += lc.tileSize) {
		discretizeTile(offset);

		scheduler->mutex.lock();
		scheduler->update();
		scheduler->mutex.unlock();
	}
}
