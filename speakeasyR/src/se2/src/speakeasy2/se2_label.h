#ifndef SE2_LABEL_H
#define SE2_LABEL_H

#include "se2_partitions.h"

void se2_find_most_specific_labels(igraph_t const* graph,
                                   igraph_vector_t const* weights,
                                   se2_partition* partition,
                                   igraph_real_t const fraction_nodes_to_label);

void se2_relabel_worst_nodes(igraph_t const* graph,
                             igraph_vector_t const* weights,
                             se2_partition* partition,
                             igraph_real_t const fraction_nodes_to_label);

void se2_burst_large_communities(igraph_t const* graph,
                                 se2_partition* partition,
                                 igraph_real_t const fraction_nodes_to_move,
                                 igraph_integer_t const min_community_size);

igraph_bool_t se2_merge_well_connected_communities(igraph_t const* graph,
    igraph_vector_t const* weights,
    se2_partition* partition,
    igraph_real_t* prev_merge_threshold);

#endif
