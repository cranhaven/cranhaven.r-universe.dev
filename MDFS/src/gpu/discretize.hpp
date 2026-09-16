#ifndef DISCRETIZE_HPP
#define DISCRETIZE_HPP

#include <stdint.h>
#include <stddef.h>

void discretize(uint32_t seed,
	uint32_t disc,
	uint32_t var,
	size_t div,
	size_t length,
	double *in_data,
	int8_t *out_data,
	float range);

#endif
