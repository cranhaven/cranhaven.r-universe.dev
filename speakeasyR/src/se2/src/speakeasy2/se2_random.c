#include "se2_random.h"

/* Initializes default igraph random number generator to use twister method */
igraph_error_t se2_rng_init(const int seed)
{
  igraph_rng_t rng;
  IGRAPH_CHECK(igraph_rng_init(&rng, &igraph_rngtype_mt19937));

  igraph_rng_set_default(&rng);
  igraph_rng_seed(igraph_rng_default(), seed);

  return IGRAPH_SUCCESS;
}

/* Shuffle the first m elements of the n element vector arr */
void se2_randperm(igraph_vector_int_t* arr, igraph_integer_t const n,
                  igraph_integer_t const m)
{
  igraph_integer_t swap = 0;
  igraph_integer_t idx = 0;
  for (igraph_integer_t i = 0; i < m; i++) {
    idx = RNG_INTEGER(0, n - 1);
    swap = VECTOR(*arr)[i];
    VECTOR(*arr)[i] = VECTOR(*arr)[idx];
    VECTOR(*arr)[idx] = swap;
  }

  return;
}
