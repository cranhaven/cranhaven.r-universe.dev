#include "scheduler.h"

static uint64_t newton(uint64_t n, uint64_t k) {
	uint64_t ret = 1;
	for (uint64_t i = n - k + 1; i <= n; i++) {
		ret *= i;
	}
	for (uint64_t i = 2; i <= k; i++) {
		ret /= i;
	}
	return ret;
}

Scheduler::Scheduler(uint64_t dim, uint64_t width, void (*pC)(int, int, int, int))
	: dim(dim), width(width), pC(pC), discretized(0),
	pdone(0), ptodo(newton(width + dim - 1, dim)) {
}

void Scheduler::update() {
	uint64_t last = discretized++;
	uint64_t off1 = width;
	uint64_t off2 = off1 * width;
	uint64_t off3 = off2 * width;
	uint64_t off4 = off3 * width;

	pC(discretized, width, pdone, ptodo);

	if (dim == 2) {
		for (uint64_t i = 0; i <= last; i++)
			packs.push_back(i * off1 + last);
	}
	if (dim == 3) {
		for (uint64_t i = 0; i <= last; i++)
		for (uint64_t j = i; j <= last; j++)
			packs.push_back(i * off2 + j * off1 + last);
	}
	if (dim == 4) {
		for (uint64_t i = 0; i <= last; i++)
		for (uint64_t j = i; j <= last; j++)
		for (uint64_t k = j; k <= last; k++)
			packs.push_back(i * off3 + j * off2 + k * off1 + last);
	}
	if (dim == 5) {
		for (uint64_t i = 0; i <= last; i++)
		for (uint64_t j = i; j <= last; j++)
		for (uint64_t k = j; k <= last; k++)
		for (uint64_t l = k; l <= last; l++)
			packs.push_back(i * off4 + j * off3 + k * off2 + l * off1 + last);
	}
}

uint64_t Scheduler::getPack() {
	uint64_t ret = packs.front();
	packs.pop_front();
	pdone++;
	pC(discretized, width, pdone, ptodo);
	return ret;
}

bool Scheduler::empty() {
	return packs.empty();
}

bool Scheduler::done() {
	return pdone == ptodo;
}
