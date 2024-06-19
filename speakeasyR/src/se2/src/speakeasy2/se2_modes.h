#ifndef SE2_MODES_H
#define SE2_MODES_H

#include <igraph_interface.h>
#include "speak_easy_2.h"
#include "se2_partitions.h"

typedef struct se2_tracker se2_tracker;

se2_tracker* se2_tracker_init(se2_options const* opts);
void se2_tracker_destroy(se2_tracker* tracker);
igraph_integer_t se2_tracker_mode(se2_tracker const* tracker);
igraph_bool_t se2_do_terminate(se2_tracker* tracker);
igraph_bool_t se2_do_save_partition(se2_tracker* tracker);
void se2_mode_run_step(igraph_t const* graph, igraph_vector_t const* weights,
                       se2_partition* partition, se2_tracker* tracker, igraph_integer_t const time);

#endif
