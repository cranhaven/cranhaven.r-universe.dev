#ifndef SE2_SEEDING_H
#define SE2_SEEDING_H

#include <igraph_interface.h>

#include "speak_easy_2.h"

igraph_integer_t se2_seeding(igraph_t const* graph,
                             igraph_vector_t const* weights,
                             igraph_vector_t const* kin, se2_options const* opts,
                             igraph_vector_int_t* ic_store);

#endif
