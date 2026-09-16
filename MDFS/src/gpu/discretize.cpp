#include "discretize.hpp"

#include <vector>
#include <random>
#include <algorithm>

// TODO: unify both discretize functions
void discretize(uint32_t seed,
	uint32_t disc,
	uint32_t var,
	size_t div,
	size_t length,
	double *in_data,
	int8_t *out_data,
	float range) {
	std::mt19937 seedGen0(seed);
	std::mt19937 seedGen1(seedGen0() ^ disc);
	std::mt19937 gen(seedGen1() ^ var);
	std::uniform_real_distribution<double> dis(1.0 - range, 1.0 + range);

	std::vector<double> data;
	data.assign(in_data, in_data + length);
	std::sort(data.begin(), data.end());

	double* thr = new double[div];
	double sum = 0.0f;
	size_t done = 0;

	for (size_t i = 0; i < div; i++) {
		thr[i] = dis(gen);
		sum += thr[i];
	}

	sum += dis(gen);

	for (size_t i = 0; i < div; i++) {
		done += std::lround((thr[i] / sum) * length);
		if (done >= length) done = length - 1;
		thr[i] = data[done];
	}

	for (size_t i = 0; i < length; i++) {
		out_data[i] = 0;
		for (size_t d = 0; d < div; d++) {
			out_data[i] += in_data[i] > thr[d];
		}
	}

	delete[] thr;
}
