#include <thread>
#include <chrono>
#include <vector>
#include "worker.h"
#include "calc.h"
#include "cudaerrchk.h"
#include "allocator.h"
#include "kernellauncher.h"
#include "datafile.h"

// std::memcpy
#include <cstring>

Worker::Worker(int vars,
	int gpuId,
	LaunchConfig lc,
	Calc* calc,
	Scheduler* scheduler,
	DataFile df) : vars(vars), gpuId(gpuId), lc(lc),
	calc(calc), scheduler(scheduler), df(df) {

	CUDA(cudaSetDevice(gpuId));

	uint64_t vol = 1;
	for (int i = 1; i < lc.dim; i++) {
		vol *= lc.tileSize * df.fi.ncls;
	}

	for (int i = 0; i < 2; i++) {
		data[i] = (uint64_t**) MALLOC_HOST(lc.dim * sizeof(uint64_t*));
		devData[i] = (uint64_t**) MALLOC_HOST(lc.dim * sizeof(uint64_t*));

		devCounters[i] = (uint64_t**) MALLOC_HOST(lc.dim * sizeof(uint64_t*));

		for (int j = 0; j < lc.dim; j++) {
			data[i][j] = (uint64_t*) MALLOC_PINNED(lc.disc * lc.tileSize * df.fi.varsize);
			devData[i][j] = (uint64_t*) MALLOC_DEVICE(lc.disc * lc.tileSize * df.fi.varsize);
			if (lc.bf == BF_SPLIT) {
				devCounters[i][j] = (uint64_t*) MALLOC_DEVICE(lc.disc * vol * sizeof(uint64_t));
			}
		}

		devIG[i] = (float*) MALLOC_DEVICE(vars * sizeof(float));
		CUDA(cudaMemset(devIG[i], 0, vars * sizeof(float)));
		IG[i] = (float*) MALLOC_HOST(vars * sizeof(float));

		CUDA(cudaStreamCreate(&stream[i]));
	}
}

Worker::~Worker() {
	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < lc.dim; j++) {
			FREE_PINNED(data[i][j]);
			FREE_DEVICE(devData[i][j]);
			if (lc.bf == BF_SPLIT) {
				FREE_DEVICE(devCounters[i][j]);
			}

		}
		FREE_HOST(data[i]);
		FREE_HOST(devData[i]);

		FREE_HOST(devCounters[i]);

		FREE_DEVICE(devIG[i]);
		FREE_HOST(IG[i]);

		//CUDA(cudaStreamDestroy(stream[i]));
	}
}

void Worker::workLoop() {
	CUDA(cudaSetDevice(gpuId));

	for (int i = 0; true; i ^= 1) {
		scheduler->mutex.lock();

		if (scheduler->done()) {
			scheduler->mutex.unlock();
			break;
		}

		if (scheduler->empty()) {
			scheduler->mutex.unlock();
			std::this_thread::sleep_for(std::chrono::milliseconds(100));
		} else {
			uint64_t packId = scheduler->getPack();
			scheduler->mutex.unlock();

			process(packId, i);
		}
	}

	for (int i = 0; i < 2; i++) {
		CUDA(cudaMemcpy(IG[i], devIG[i], vars * sizeof(float), cudaMemcpyDeviceToHost));
		calc->returnResults(IG[i]);
	}
}

void Worker::process(uint64_t packId, int strNum) {
	std::vector<int> offset = decodePackId(packId);

	std::size_t varLen = df.fi.varlen0 + df.fi.varlen1;
	std::size_t discLen = df.fi.nvar * varLen;
	std::size_t tileLen = lc.tileSize * varLen;
	for (std::size_t i = 0; i < lc.dim; i++) {
		for (std::size_t j = 0; j < lc.disc; j++) {
			std::memcpy(data[strNum][i] + (j * tileLen),
				df.data + (j * discLen + offset[i] * tileLen),
				tileLen * sizeof(uint64_t));
		}
	}

	CUDA(cudaStreamSynchronize(stream[strNum]));

	for (int i = 0; i < lc.dim; i++) {
		CUDA(cudaMemcpyAsync(devData[strNum][i],
			data[strNum][i],
			lc.disc * tileLen * sizeof(uint64_t),
			cudaMemcpyHostToDevice, stream[strNum]));
	}

	if (lc.bf == BF_SPLIT) {
		for (int i = 0; i < lc.dim; i++) {
			lowerDimCounters(i, offset, strNum);
		}
	}

	mainKernelLauncher(vars, df.fi, lc, offset,
		devData[strNum],
		devCounters[strNum],
		devIG[strNum],
		stream[strNum]);
}

std::vector<int> Worker::decodePackId(uint64_t packId) {
	uint64_t width = df.fi.nvar / lc.tileSize;
	std::vector<int> res;

	for (int i = 0; i < lc.dim; i++) {
		res.push_back(packId % width);
		packId /= width;
	}

	return res;
}

void Worker::lowerDimCounters(int ix, std::vector<int> offset, int strNum) {
	if (lc.dim == 2) {
		uint64_t len = lc.tileSize * df.fi.ncls;
		for (uint64_t d = 0; d < lc.disc; d++) {
			CUDA(cudaMemcpyAsync(devCounters[strNum][ix] + d * len,
				df.counters + (d * df.fi.nvar * df.fi.ncls + offset[ix] * lc.tileSize * df.fi.ncls),
				len * sizeof(uint64_t),
				cudaMemcpyHostToDevice, stream[strNum]));
		}
		return;
	}

	tablesKernelLauncher(vars, df.fi, lc, ix, offset,
		devData[strNum],
		devCounters[strNum],
		stream[strNum]);
}


