#ifndef LAUNCHCONFIG_CUH
#define LAUNCHCONFIG_CUH

#include "datafile.h"

enum ReduceMethod { RM_AVG, RM_MAX };

struct LaunchConfig {
	int tileSize;
	int dim;
	int div;
	int disc;
	float range;
	float pseudo;
	uint32_t seed;
	ReduceMethod rm;
	BinaryFormat bf;
};

#endif
