#ifndef SE2_RANDOM_H
#define SE2_RANDOM_H

#include <igraph_random.h>

igraph_error_t se2_rng_init(const int seed);
void se2_randperm(igraph_vector_int_t* arr, igraph_integer_t const n,
                  igraph_integer_t const m);

#endif
