#ifndef SPEAK_EASY_H
#define SPEAK_EASY_H

#include "igraph_datatype.h"
#include "igraph_vector.h"
#include "igraph_matrix.h"

typedef struct {
  igraph_integer_t independent_runs;  // Number of independent runs to perform.
  igraph_integer_t subcluster;        // Depth of clustering.
  igraph_integer_t
  multicommunity;    // Max number of communities a node can be a member of.
  igraph_integer_t
  target_partitions; // Number of partitions to find per independent run.
  igraph_integer_t target_clusters;   // Expected number of clusters to find.
  igraph_integer_t minclust;          // Minimum cluster size to subclustering.
  igraph_integer_t
  discard_transient; // How many initial partitions to discard before recording.
  igraph_integer_t random_seed;       // Seed for reproducing results.
  igraph_integer_t max_threads;       // Number of threads to use.
  bool node_confidence;
  bool verbose; // Print information to stdout
} se2_options;

igraph_error_t speak_easy_2(igraph_t* graph, igraph_vector_t* weights,
                            se2_options* opts, igraph_matrix_int_t* res);
igraph_error_t se2_order_nodes(igraph_t const* graph,
                               igraph_vector_t const* weights,
                               igraph_matrix_int_t const* memb,
                               igraph_matrix_int_t* ordering);
#endif
