#ifdef SE2PAR
#  include <pthread.h>
#endif

#include <igraph_error.h>
#include <igraph_structural.h>
#include <igraph_community.h>
#include <igraph_constructors.h>

#include "speak_easy_2.h"
#include "se2_print.h"
#include "se2_seeding.h"
#include "se2_random.h"
#include "se2_modes.h"
#include "se2_reweight_graph.h"

igraph_bool_t greeting_printed = false;

#define SE2_SET_OPTION(opts, field, default) \
    (opts->field) = (opts)->field ? (opts)->field : (default)

static igraph_error_t se2_core(igraph_t const* graph,
                               igraph_vector_t const* weights,
                               igraph_vector_int_list_t* partition_list,
                               igraph_integer_t const partition_offset,
                               se2_options const* opts)
{
  igraph_error_t errorcode = IGRAPH_SUCCESS;
  se2_tracker* tracker = se2_tracker_init(opts);
  igraph_vector_int_t* ic_store =
    igraph_vector_int_list_get_ptr(partition_list, partition_offset);
  se2_partition* working_partition = se2_partition_init(graph, ic_store);
  if (!working_partition) {
    IGRAPH_ERROR("Failed to generate partition.", IGRAPH_EINVAL);
  }

  igraph_integer_t partition_idx = partition_offset;
  for (igraph_integer_t time = 0; !se2_do_terminate(tracker); time++) {
    se2_mode_run_step(graph, weights, working_partition, tracker, time);
    if (se2_do_save_partition(tracker)) {
      se2_partition_store(working_partition, partition_list, partition_idx);
      partition_idx++;
    }
  }

  se2_tracker_destroy(tracker);
  se2_partition_destroy(working_partition);

  return errorcode;
}

struct represent_parameters {
  igraph_integer_t tid;
  se2_options* opts;
  igraph_integer_t n_partitions;
  igraph_vector_int_list_t* partition_store;
  igraph_matrix_t* nmi_sum_accumulator;
};

static void* se2_thread_mrp(void* parameters)
{
  struct represent_parameters* p = (struct represent_parameters*)parameters;
  igraph_real_t nmi;

  igraph_integer_t n_threads = p->opts->max_threads;
  for (igraph_integer_t i = p->tid; i < p->n_partitions; i += n_threads) {
    for (igraph_integer_t j = (i + 1); j < p->n_partitions; j++) {
      igraph_compare_communities(
        igraph_vector_int_list_get_ptr(p->partition_store, i),
        igraph_vector_int_list_get_ptr(p->partition_store, j),
        &nmi,
        IGRAPH_COMMCMP_NMI);
      MATRIX(* p->nmi_sum_accumulator, i, p->tid) += nmi;
      MATRIX(* p->nmi_sum_accumulator, j, p->tid) += nmi;
    }
  }

  return NULL;
}

static void se2_most_representative_partition(igraph_vector_int_list_t const
    *partition_store, igraph_integer_t const n_partitions,
    igraph_vector_int_t* most_representative_partition,
    se2_options const* opts, igraph_integer_t const subcluster)
{
  igraph_vector_int_t* selected_partition;
  igraph_matrix_t nmi_sum_accumulator;
  igraph_vector_t nmi_sums;
  igraph_integer_t idx = 0;
  igraph_real_t max_nmi = -1;
  igraph_real_t mean_nmi = 0;

  igraph_matrix_init( &nmi_sum_accumulator, n_partitions, opts->max_threads);
  igraph_vector_init( &nmi_sums, n_partitions);

  struct represent_parameters args[opts->max_threads];

#ifdef SE2PAR
  pthread_t threads[opts->max_threads];
#endif
  for (igraph_integer_t tid = 0; tid < opts->max_threads; tid++) {
    args[tid].tid = tid;
    args[tid].opts = (se2_options*)opts;
    args[tid].n_partitions = n_partitions;
    args[tid].partition_store = (igraph_vector_int_list_t*)partition_store;
    args[tid].nmi_sum_accumulator = &nmi_sum_accumulator;

#ifdef SE2PAR
    pthread_create( &threads[tid], NULL, se2_thread_mrp, (void*) &args[tid]);
#else
    se2_thread_mrp((void*) &args[tid]);
#endif
  }

#ifdef SE2PAR
  for (igraph_integer_t tid = 0; tid < opts->max_threads; tid++) {
    pthread_join(threads[tid], NULL);
  }
#endif

  igraph_matrix_rowsum( &nmi_sum_accumulator, &nmi_sums);

  if (opts->verbose && (subcluster == 0)) {
    mean_nmi = igraph_matrix_sum( &nmi_sum_accumulator);
    mean_nmi /= (n_partitions* (n_partitions - 1));
    se2_printf("Mean of all NMIs is %0.5f.\n", mean_nmi);
  }

  for (igraph_integer_t i = 0; i < n_partitions; i++) {
    if (VECTOR(nmi_sums)[i] > max_nmi) {
      max_nmi = VECTOR(nmi_sums)[i];
      idx = i;
    }
  }

  igraph_matrix_destroy( &nmi_sum_accumulator);
  igraph_vector_destroy( &nmi_sums);

  selected_partition = igraph_vector_int_list_get_ptr(partition_store, idx);

  igraph_integer_t n_nodes = igraph_vector_int_size(selected_partition);
  for (igraph_integer_t i = 0; i < n_nodes; i++) {
    VECTOR(* most_representative_partition)[i] = VECTOR(* selected_partition)[i];
  }
}

struct bootstrap_params {
  igraph_integer_t tid;
  igraph_integer_t n_nodes;
  igraph_t* graph;
  igraph_vector_t* weights;
  igraph_integer_t subcluster_iter;
  igraph_vector_t* kin;
  igraph_vector_int_list_t* partition_store;
  se2_options* opts;
#ifdef SE2PAR
  pthread_mutex_t* print_mutex;
#endif
  igraph_vector_int_t* memb;
};

static void* se2_thread_bootstrap(void* parameters)
{
  struct bootstrap_params const* p = (struct bootstrap_params*)parameters;

  igraph_integer_t n_threads = p->opts->max_threads;
  igraph_integer_t independent_runs = p->opts->independent_runs;
  for (igraph_integer_t run_i = p->tid; run_i < independent_runs;
       run_i += n_threads) {
    igraph_integer_t partition_offset = run_i* p->opts->target_partitions;
    igraph_vector_int_t ic_store;
    igraph_vector_int_init( &ic_store, p->n_nodes);

    se2_rng_init(run_i + p->opts->random_seed);
    igraph_integer_t n_unique = se2_seeding(p->graph, p->weights, p->kin,
                                            p->opts, &ic_store);
    igraph_vector_int_list_set(p->partition_store, partition_offset, &ic_store);

    if ((p->opts->verbose) && (!p->subcluster_iter)) {
#ifdef SE2PAR
      pthread_mutex_lock(p->print_mutex);
#endif

      if (!greeting_printed) {
        greeting_printed = true;
        se2_printf("Completed generating initial labels.\n"
                   "Produced %"IGRAPH_PRId" seed labels, "
                   "while goal was %"IGRAPH_PRId".\n\n"
                   "Starting level 1 clustering",
                   n_unique, p->opts->target_clusters);

        if (p->opts->max_threads > 1) {
          se2_printf("; independent runs might not be displayed in order - "
                     "that is okay...\n");
        } else {
          se2_printf("...\n");
        }
      }

      se2_printf("Starting independent run #%"IGRAPH_PRId" of %"IGRAPH_PRId"\n",
                 run_i + 1, p->opts->independent_runs);

#ifdef SE2PAR
      pthread_mutex_unlock(p->print_mutex);
#endif
    }

    se2_core(p->graph, p->weights, p->partition_store, partition_offset, p->opts);
  }

  return NULL;
}

static void se2_bootstrap(igraph_t* graph,
                          igraph_vector_t const* weights,
                          igraph_integer_t const subcluster_iter,
                          se2_options const* opts,
                          igraph_vector_int_t* memb)
{
  igraph_integer_t n_nodes = igraph_vcount(graph);
  igraph_vector_t kin;
  igraph_integer_t n_partitions = opts->target_partitions*
                                  opts->independent_runs;
  igraph_vector_int_list_t partition_store;

  igraph_vector_init( &kin, n_nodes);
  igraph_strength(graph, &kin, igraph_vss_all(), IGRAPH_IN, IGRAPH_LOOPS,
                  weights);

  igraph_vector_int_list_init( &partition_store, n_partitions);

  if ((opts->verbose) && (!subcluster_iter) && (opts->multicommunity > 1)) {
    se2_puts("Attempting overlapping clustering.");
  }

#ifdef SE2PAR
  pthread_t threads[opts->max_threads];
  pthread_mutex_t print_mutex;
  pthread_mutex_init( &print_mutex, NULL);
#endif

  struct bootstrap_params args[opts->max_threads];

  for (igraph_integer_t tid = 0; tid < opts->max_threads; tid++) {
    args[tid].tid = tid;
    args[tid].n_nodes = n_nodes;
    args[tid].graph = graph;
    args[tid].weights = (igraph_vector_t*)weights;
    args[tid].subcluster_iter = subcluster_iter;
    args[tid].kin = &kin;
    args[tid].partition_store = &partition_store;
    args[tid].opts = (se2_options*)opts;
#ifdef SE2PAR
    args[tid].print_mutex = &print_mutex;
#endif
    args[tid].memb = memb;

#ifdef SE2PAR
    pthread_create( &threads[tid], NULL, se2_thread_bootstrap,
                    (void*) &args[tid]);
#else
    se2_thread_bootstrap((void*) &args[tid]);
#endif
  }

#ifdef SE2PAR
  for (igraph_integer_t tid = 0; tid < opts->max_threads; tid++) {
    pthread_join(threads[tid], NULL);
  }

  pthread_mutex_destroy( &print_mutex);
#endif

  if ((opts->verbose) && (!subcluster_iter)) {
    se2_printf("\nGenerated %"IGRAPH_PRId" partitions at level 1.\n",
               n_partitions);
  }

  se2_most_representative_partition( &partition_store,
                                     n_partitions,
                                     memb, opts, subcluster_iter);

  igraph_vector_int_list_destroy( &partition_store);
  igraph_vector_destroy( &kin);
}

static igraph_integer_t default_target_clusters(igraph_t const* graph)
{
  igraph_integer_t n_nodes = igraph_vcount(graph);

  if (n_nodes < 10) {
    return n_nodes;
  }

  if ((n_nodes / 100) < 10) {
    return 10;
  }

  return n_nodes / 100;
}

static igraph_integer_t default_max_threads(igraph_integer_t const runs)
{
  igraph_integer_t n_threads = 1;
#ifdef SE2PAR
  n_threads = runs;
#endif
  return n_threads;
}

static void se2_set_defaults(igraph_t const* graph, se2_options* opts)
{
  SE2_SET_OPTION(opts, independent_runs, 10);
  SE2_SET_OPTION(opts, subcluster, 1);
  SE2_SET_OPTION(opts, multicommunity, 1);
  SE2_SET_OPTION(opts, target_partitions, 5);
  SE2_SET_OPTION(opts, target_clusters, default_target_clusters(graph));
  SE2_SET_OPTION(opts, minclust, 5);
  SE2_SET_OPTION(opts, discard_transient, 3);
  SE2_SET_OPTION(opts, random_seed, RNG_INTEGER(1, 9999));
  SE2_SET_OPTION(opts, max_threads,
                 default_max_threads(opts->independent_runs));
  SE2_SET_OPTION(opts, node_confidence, false);
  SE2_SET_OPTION(opts, verbose, false);
}

static void se2_collect_community_members(igraph_vector_int_t const* memb,
    igraph_vector_int_t* idx, igraph_integer_t const comm)
{
  igraph_integer_t n_memb = 0;
  for (igraph_integer_t i = 0; i < igraph_vector_int_size(memb); i++) {
    n_memb += VECTOR(* memb)[i] == comm;
  }

  igraph_vector_int_init(idx, n_memb);
  igraph_integer_t count = 0;
  for (igraph_integer_t i = 0; i < igraph_vector_int_size(memb); i++) {
    if (VECTOR(* memb)[i] == comm) {
      VECTOR(* idx)[count] = i;
      count++;
    }
  }
}

static inline igraph_bool_t se2_edge_in_community(igraph_integer_t const
    to, igraph_integer_t const from, igraph_vector_int_t const* members)
{
  return igraph_vector_int_binsearch2(members, to) &&
         igraph_vector_int_binsearch2(members, from);
}

/* Repack edges so node IDs are sequential. */
static void se2_reindex_edges(igraph_vector_int_t* edges)
{
  igraph_vector_int_t ids;
  igraph_integer_t const vmin = igraph_vector_int_min(edges);
  igraph_integer_t const n_nodes = 1 + igraph_vector_int_max(edges) -
                                   igraph_vector_int_min(edges);

  igraph_vector_int_init( &ids, n_nodes);
  for (igraph_integer_t i = 0; i < igraph_vector_int_size(edges); i++) {
    VECTOR(ids)[VECTOR(* edges)[i] - vmin]++;
  }

  igraph_integer_t shift = 0;
  for (igraph_integer_t i = 0; i < igraph_vector_int_size( &ids); i++) {
    if (VECTOR(ids)[i] == 0) {
      shift++;
    } else {
      VECTOR(ids)[i] = shift;
    }
  }

  for (igraph_integer_t i = 0; i < igraph_vector_int_size(edges); i++) {
    VECTOR(* edges)[i] -= VECTOR(ids)[VECTOR(* edges)[i] - vmin] + vmin;
  }

  igraph_vector_int_destroy( &ids);
}

static void se2_subgraph_from_community(igraph_t const* origin,
                                        igraph_vector_t const* origin_weights,
                                        igraph_t* subgraph,
                                        igraph_vector_t* sub_weights,
                                        igraph_vector_int_t const* members)
{
  igraph_real_t const density = (igraph_real_t)igraph_ecount(origin) /
                                igraph_vcount(origin);
  igraph_vector_int_t edges, edge_ids;
  igraph_integer_t n_edges = ceil(density* igraph_vector_int_size(members));
  igraph_eit_t eit;

  igraph_vector_int_init( &edge_ids, n_edges);

  igraph_eit_create(origin, igraph_ess_all(IGRAPH_EDGEORDER_ID), &eit);
  igraph_integer_t edge_count = 0;
  while (!IGRAPH_EIT_END(eit)) {
    if (edge_count == n_edges) {
      n_edges *= 2;
      igraph_vector_int_resize( &edge_ids, n_edges);
    }

    igraph_integer_t eid = IGRAPH_EIT_GET(eit);
    if (se2_edge_in_community(IGRAPH_TO(origin, eid), IGRAPH_FROM(origin, eid),
                              members)) {
      VECTOR(edge_ids)[edge_count] = eid;
      edge_count++;
    }

    IGRAPH_EIT_NEXT(eit);
  }
  igraph_vector_int_resize( &edge_ids, edge_count);

  if (origin_weights) {
    igraph_vector_init(sub_weights, edge_count);
    for (igraph_integer_t i = 0; i < edge_count; i++) {
      VECTOR(* sub_weights)[i] = VECTOR(* origin_weights)[VECTOR(edge_ids)[i]];
    }
  } else {
    igraph_vector_init(sub_weights, 0);
  }

  igraph_vector_int_init( &edges, edge_count * 2);
  for (igraph_integer_t i = 0; i < edge_count; i++) {
    igraph_integer_t to, from;
    igraph_edge(origin, VECTOR(edge_ids)[i], &from, &to);
    VECTOR(edges)[i * 2] = from;
    VECTOR(edges)[1 + (i * 2)] = to;
  }

  se2_reindex_edges( &edges);
  igraph_create(subgraph, &edges, igraph_vector_int_size(members),
                igraph_is_directed(origin));

  igraph_vector_int_destroy( &edge_ids);
  igraph_vector_int_destroy( &edges);
  igraph_eit_destroy( &eit);
}

/* For hierarchical clustering, each community from the previous level gets
clustered. Each of these clusters gets a "private scope" set of labels starting
at 0. These must be relabeled to a global scope. */
static void se2_relabel_hierarchical_communities(igraph_vector_int_t const*
    prev_membs, igraph_vector_int_t* level_membs)
{
  igraph_integer_t const n_comms = igraph_vector_int_max(prev_membs) -
                                   igraph_vector_int_min(prev_membs);

  igraph_integer_t prev_max = 0;
  igraph_integer_t curr_max = 0;
  for (igraph_integer_t i = 0; i < n_comms; i++) {
    igraph_vector_int_t member_ids;
    se2_collect_community_members(prev_membs, &member_ids, i);
    for (igraph_integer_t j = 0; j < igraph_vector_int_size( &member_ids); j++) {
      igraph_integer_t local_label = VECTOR(* level_membs)[VECTOR(member_ids)[j]];

      VECTOR(* level_membs)[VECTOR(member_ids)[j]] += prev_max;
      if ((local_label + prev_max) > curr_max) {
        curr_max = local_label + prev_max;
      }
    }
    prev_max = curr_max + 1;
    igraph_vector_int_destroy( &member_ids);
  }
}

igraph_error_t speak_easy_2(igraph_t* graph, igraph_vector_t* weights,
                            se2_options* opts, igraph_matrix_int_t* memb)
{
  se2_set_defaults(graph, opts);

#ifndef SE2PAR
  if (opts->max_threads > 1) {
    se2_warn("SpeakEasy 2 was not compiled with thread support. "
             "Ignoring `max_threads`.\n\n"
             "To suppress this warning do not set `max_threads`.");
    opts->max_threads = 1;
  }
#endif

  if (opts->verbose) {
    igraph_bool_t isweighted = false;
    if (weights) {
      for (igraph_integer_t i = 0; i < igraph_ecount(graph); i++) {
        if (VECTOR(* weights)[i] != 1) {
          isweighted = true;
          break;
        }
      }
    }

    igraph_integer_t possible_edges = igraph_vcount(graph);
    possible_edges *= possible_edges;
    igraph_real_t edge_density = (igraph_real_t)igraph_ecount(graph) /
                                 possible_edges;
    igraph_bool_t directed = igraph_is_directed(graph);
    edge_density *= (!directed + 1);
    se2_printf("Approximate edge density is %g.\n"
               "Input type treated as %s.\n"
               "Graph is %s.\n\n"
               "Calling main routine at level 1.\n",
               edge_density, isweighted ? "weighted" : "unweighted",
               directed ? "asymmetric" : "symmetric");
  }

  igraph_matrix_int_init(memb, opts->subcluster, igraph_vcount(graph));

  igraph_vector_int_t level_memb;
  igraph_vector_int_init( &level_memb, igraph_vcount(graph));
  se2_reweight(graph, weights);
  se2_bootstrap(graph, weights, 0, opts, &level_memb);
  igraph_matrix_int_set_row(memb, &level_memb, 0);

  for (igraph_integer_t level = 1; level < opts->subcluster; level++) {
    if (opts->verbose) {
      se2_printf("\nSubclustering at level %"IGRAPH_PRId".\n", level + 1);
    }

    igraph_vector_int_t prev_memb;
    igraph_vector_int_init( &prev_memb, igraph_matrix_int_ncol(memb));
    igraph_matrix_int_get_row(memb, &prev_memb, level - 1);

    igraph_integer_t const n_comms = igraph_vector_int_max( &prev_memb) -
                                     igraph_vector_int_min( &prev_memb);
    for (igraph_integer_t comm = 0; comm < n_comms; comm++) {
      igraph_vector_int_t member_ids;
      se2_collect_community_members( &prev_memb, &member_ids, comm);
      igraph_integer_t const n_membs = igraph_vector_int_size( &member_ids);

      if (n_membs <= opts->minclust) {
        for (igraph_integer_t i = 0; i < n_membs; i++) {
          VECTOR(level_memb)[VECTOR(member_ids)[i]] = 0;
        }

        igraph_vector_int_destroy( &member_ids);
        continue;
      }

      igraph_t subgraph;
      igraph_vector_t subgraph_weights;
      igraph_vector_t* subgraph_weights_ptr = NULL;
      igraph_vector_int_t subgraph_memb;

      igraph_vector_int_init( &subgraph_memb, n_membs);
      se2_subgraph_from_community(graph, weights, &subgraph, &subgraph_weights,
                                  &member_ids);

      if (igraph_vector_size( &subgraph_weights) > 0) {
        subgraph_weights_ptr = &subgraph_weights;
      }

      se2_reweight( &subgraph, subgraph_weights_ptr);
      se2_bootstrap( &subgraph, subgraph_weights_ptr, level, opts,
                     &subgraph_memb);

      for (igraph_integer_t i = 0; i < igraph_vector_int_size( &subgraph_memb);
           i++) {
        VECTOR(level_memb)[VECTOR(member_ids)[i]] = VECTOR(subgraph_memb)[i];
      }

      igraph_vector_int_destroy( &member_ids);
      igraph_vector_destroy( &subgraph_weights);
      igraph_vector_int_destroy( &subgraph_memb);
      igraph_destroy( &subgraph);
    }

    se2_relabel_hierarchical_communities( &prev_memb, &level_memb);
    igraph_matrix_int_set_row(memb, &level_memb, level);
    igraph_vector_int_destroy( &prev_memb);
  }

  igraph_vector_int_destroy( &level_memb);

  if (opts->verbose) {
    se2_printf("\n");
  }

  return IGRAPH_SUCCESS;
}

static void se2_order_nodes_i(igraph_matrix_int_t const* memb,
                              igraph_vector_int_t* initial,
                              igraph_matrix_int_t* ordering,
                              igraph_integer_t const level,
                              igraph_integer_t const start,
                              igraph_integer_t const len)
{
  if (len == 0) {
    return;
  }

  if (level == igraph_matrix_int_nrow(memb)) {
    return;
  }

  igraph_vector_int_t comm_sizes;
  igraph_vector_int_t pos;

  igraph_integer_t comm_min = IGRAPH_INTEGER_MAX;
  igraph_integer_t comm_max = 0;
  for (igraph_integer_t i = 0; i < len; i++) {
    if (MATRIX(* memb, level, VECTOR(* initial)[start + i]) < comm_min) {
      comm_min = MATRIX(* memb, level, VECTOR(* initial)[start + i]);
    }

    if (MATRIX(* memb, level, VECTOR(* initial)[start + i]) > comm_max) {
      comm_max = MATRIX(* memb, level, VECTOR(* initial)[start + i]);
    }
  }

  igraph_integer_t const n_communities = comm_max - comm_min + 1;
  igraph_vector_int_init( &comm_sizes, n_communities);
  igraph_vector_int_init( &pos, n_communities);

  for (igraph_integer_t i = 0; i < len; i++) {
    VECTOR(comm_sizes)[MATRIX(* memb, level,
                              VECTOR(* initial)[start + i]) - comm_min]++;
  }

  igraph_vector_int_t indices;
  igraph_vector_int_init( &indices, n_communities);
  igraph_vector_int_qsort_ind( &comm_sizes, &indices, IGRAPH_DESCENDING);

  VECTOR(pos)[VECTOR(indices)[0]] = start;
  for (igraph_integer_t i = 1; i < n_communities; i++) {
    VECTOR(pos)[VECTOR(indices)[i]] = VECTOR(pos)[VECTOR(indices)[i - 1]] +
                                      VECTOR(comm_sizes)[VECTOR(indices)[i - 1]];
  }

  for (igraph_integer_t i = 0; i < len; i++) {
    igraph_integer_t comm = MATRIX(* memb, level, VECTOR(* initial)[start + i]) -
                            comm_min;
    MATRIX(* ordering, level, VECTOR(pos)[comm]) = VECTOR(* initial)[start + i];
    VECTOR(pos)[comm]++;
  }
  igraph_vector_int_destroy( &pos);

  for (igraph_integer_t i = 0; i < len; i++) {
    VECTOR(* initial)[start + i] = MATRIX(* ordering, level, start + i);
  }

  igraph_integer_t comm_start = start;
  for (igraph_integer_t i = 0; i < n_communities; i++) {
    igraph_integer_t comm_len = VECTOR(comm_sizes)[VECTOR(indices)[i]];
    se2_order_nodes_i(memb, initial, ordering, level + 1, comm_start, comm_len);
    comm_start += comm_len;
  }
  igraph_vector_int_destroy( &comm_sizes);
  igraph_vector_int_destroy( &indices);
}

/* Return node indices of each cluster in order from largest-to-smallest
   community. This can be used to display community structure in heat maps. */
igraph_error_t se2_order_nodes(igraph_t const* graph,
                               igraph_vector_t const* weights,
                               igraph_matrix_int_t const* memb,
                               igraph_matrix_int_t* ordering)
{
  igraph_integer_t const n_nodes = igraph_matrix_int_ncol(memb);
  igraph_vector_t degrees;
  igraph_vector_init( &degrees, n_nodes);
  igraph_matrix_int_init(ordering, igraph_matrix_int_nrow(memb), n_nodes);
  igraph_strength(graph, &degrees, igraph_vss_all(), IGRAPH_ALL, IGRAPH_LOOPS,
                  weights);

  // Ensure nodes are ordered by highest-lowest degree within communities.
  igraph_vector_int_t init_ordering;
  igraph_vector_int_init( &init_ordering, n_nodes);

  igraph_vector_qsort_ind( &degrees, &init_ordering, IGRAPH_DESCENDING);

  igraph_vector_destroy( &degrees);

  se2_order_nodes_i(memb, &init_ordering, ordering, 0, 0, n_nodes);
  igraph_vector_int_destroy( &init_ordering);

  return IGRAPH_SUCCESS;
}
