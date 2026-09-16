#ifndef SCHEDULER_CUH
#define SCHEDULER_CUH

#include <stdint.h>
#include <deque>
#include <mutex>

class Scheduler {
private:
	uint64_t dim;
	uint64_t width;
	void (*pC)(int, int, int, int);
	uint64_t discretized;
	uint64_t pdone;
	uint64_t ptodo;
	std::deque<uint64_t> packs;
public:
	std::mutex mutex;

	Scheduler(uint64_t dim,
		uint64_t width,
		void (*pC)(int, int, int, int));

	uint64_t getPack();
	void update();
	bool empty();
	bool done();
};

#endif
