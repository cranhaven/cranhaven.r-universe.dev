#ifndef DISCRETIZER_CUH
#define DISCRETIZER_CUH

#include <stdint.h>
#include "scheduler.h"
#include "launchable.h"
#include "launchconfig.h"

class Discretizer : public Launchable {
	LaunchConfig lc;
	InputFile inf;
	DataFile df;
	Scheduler *scheduler;

	int8_t* buffer;

	void discretizeTile(int offset);
	void count(int d, int var);
	void shiftCopy(int d, int var);
	void splitCopy(int d, int var);
public:
	Discretizer(LaunchConfig lc,
		InputFile inf,
		Scheduler *scheduler);
	~Discretizer();

	DataFile getDataFile();
	void workLoop();
};

#endif
