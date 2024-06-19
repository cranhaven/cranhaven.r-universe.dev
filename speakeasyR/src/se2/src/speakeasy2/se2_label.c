#include <igraph_structural.h>
#include <igraph_random.h>
#include "se2_label.h"

static inline void global_label_proportions(igraph_t const* graph,
    igraph_vector_t const* weights, se2_partition const* partition,
    igraph_vector_t* labels_heard, igraph_integer_t const n_labels)
{
  igraph_vector_t kout;
  igraph_integer_t n_nodes = igraph_vcount(graph);
  igraph_integer_t acc = 0;

  igraph_vector_init(&kout, n_nodes);
  igraph_strength(graph, &kout, igraph_vss_all(), IGRAPH_OUT, IGRAPH_LOOPS,
                  weights);

  for (igraph_integer_t i = 0; i < n_nodes; i++) {
    VECTOR(*labels_heard)[LABEL(*partition)[i]] += VECTOR(kout)[i];
  }
  igraph_vector_destroy(&kout);

  for (igraph_integer_t i = 0; i < n_labels; i++) {
    acc += VECTOR(*labels_heard)[i];
  }

  for (igraph_integer_t i = 0; i < n_labels; i++) {
    VECTOR(*labels_heard)[i] /= acc;
  }
}

static inline igraph_real_t edge_get_weight(igraph_t const* graph,
    igraph_vector_t const* weights, igraph_integer_t const from,
    igraph_integer_t const to, igraph_bool_t const directed)
{
  if (!weights) {
    return 1;
  }

  igraph_integer_t eid;
  igraph_get_eid(graph, &eid, from, to, directed, false);
  return VECTOR(*weights)[eid];
}

static inline void local_label_proportions(igraph_t const* graph,
    igraph_vector_t const* weights, se2_partition const* partition,
    igraph_integer_t const node_id, igraph_vector_t* labels_heard,
    igraph_real_t* kin, igraph_integer_t n_labels)
{
  igraph_vector_int_t neighbors;
  igraph_integer_t n_neighbors;
  igraph_integer_t neighbor;
  igraph_real_t weight;
  igraph_bool_t directed = igraph_is_directed(graph);

  igraph_vector_int_init(&neighbors, 0);
  igraph_neighbors(graph, &neighbors, node_id, IGRAPH_IN);
  n_neighbors = igraph_vector_int_size(&neighbors);
  for (igraph_integer_t i = 0; i < n_neighbors; i++) {
    neighbor = VECTOR(neighbors)[i];
    weight = edge_get_weight(graph, weights, neighbor, node_id, directed);
    VECTOR(*labels_heard)[LABEL(*partition)[neighbor]] += weight;
  }
  igraph_vector_int_destroy(&neighbors);

  *kin = 0;
  for (igraph_integer_t i = 0; i < n_labels; i++) {
    *kin += VECTOR(*labels_heard)[i];
  }
}

/* Scores labels based on the difference between the local and global
 frequencies.  Labels that are overrepresented locally are likely to be of
 importance in tagging a node. */
static void se2_find_most_specific_labels_i(igraph_t const* graph,
    igraph_vector_t const* weights,
    se2_partition* partition,
    se2_iterator* node_iter)
{
  igraph_integer_t max_label = se2_partition_max_label(partition);
  igraph_vector_t labels_expected;
  igraph_vector_t labels_observed;
  se2_iterator* label_iter = se2_iterator_random_label_init(partition, false);
  igraph_real_t node_kin = 0;
  igraph_real_t label_specificity = 0, best_label_specificity = 0;
  igraph_integer_t best_label = -1;

  igraph_vector_init(&labels_expected, max_label + 1);
  igraph_vector_init(&labels_observed, max_label + 1);

  global_label_proportions(graph, weights, partition, &labels_expected,
                           max_label + 1);

  igraph_integer_t node_id = 0, label_id = 0;
  while ((node_id = se2_iterator_next(node_iter)) != -1) {
    igraph_vector_null(&labels_observed);
    local_label_proportions(graph, weights, partition, node_id,
                            &labels_observed, &node_kin, max_label + 1);
    while ((label_id = se2_iterator_next(label_iter)) != -1) {
      label_specificity = VECTOR(labels_observed)[label_id] -
                          (node_kin * VECTOR(labels_expected)[label_id]);
      if ((best_label == -1) || (label_specificity >= best_label_specificity)) {
        best_label_specificity = label_specificity;
        best_label = label_id;
      }
    }
    se2_partition_add_to_stage(partition, node_id, best_label,
                               best_label_specificity);
    best_label = -1;
    se2_iterator_shuffle(label_iter);
  }
  se2_partition_commit_changes(partition);

  se2_iterator_destroy(label_iter);
  igraph_vector_destroy(&labels_expected);
  igraph_vector_destroy(&labels_observed);
}

void se2_find_most_specific_labels(igraph_t const* graph,
                                   igraph_vector_t const* weights,
                                   se2_partition* partition,
                                   igraph_real_t const fraction_nodes_to_label)
{
  se2_iterator* node_iter = se2_iterator_random_node_init(partition,
                            fraction_nodes_to_label);
  se2_find_most_specific_labels_i(graph, weights, partition, node_iter);
  se2_iterator_destroy(node_iter);
}

void se2_relabel_worst_nodes(igraph_t const* graph,
                             igraph_vector_t const* weights,
                             se2_partition* partition,
                             igraph_real_t const fraction_nodes_to_label)
{
  igraph_integer_t n_nodes = igraph_vcount(graph);
  igraph_integer_t n_nodes_to_relabel = fraction_nodes_to_label * n_nodes;
  se2_iterator* node_iter = se2_iterator_k_worst_fit_nodes_init(partition,
                            n_nodes_to_relabel);

  se2_find_most_specific_labels_i(graph, weights, partition, node_iter);
  se2_iterator_destroy(node_iter);
}

void se2_burst_large_communities(igraph_t const* graph,
                                 se2_partition* partition,
                                 igraph_real_t const fraction_nodes_to_move,
                                 igraph_integer_t const min_community_size)
{
  se2_iterator* node_iter = se2_iterator_k_worst_fit_nodes_init(partition,
                            igraph_vcount(graph) * fraction_nodes_to_move);
  igraph_integer_t desired_community_size =
    se2_partition_median_community_size(partition);
  igraph_vector_int_t n_new_tags_cum;
  igraph_vector_int_t n_nodes_to_move;
  igraph_vector_int_t new_tags;
  igraph_integer_t node_id;

  igraph_vector_int_init(&n_new_tags_cum, partition->max_label + 2);
  igraph_vector_int_init(&n_nodes_to_move, partition->max_label + 1);
  while ((node_id = se2_iterator_next(node_iter)) != -1) {
    if (se2_partition_community_size(partition, LABEL(*partition)[node_id]) >=
        min_community_size) {
      VECTOR(n_nodes_to_move)[LABEL(*partition)[node_id]]++;
    }
  }

  igraph_integer_t n_new_tags;
  for (igraph_integer_t i = 0; i <= partition->max_label; i++) {
    if (VECTOR(n_nodes_to_move)[i] == 0) {
      continue;
    }

    n_new_tags = VECTOR(n_nodes_to_move)[i] / desired_community_size;
    if (n_new_tags < 2) {
      n_new_tags = 2;
    } else if (n_new_tags > 10) {
      n_new_tags = 10;
    }

    VECTOR(n_new_tags_cum)[i + 1] = n_new_tags;
  }

  for (igraph_integer_t i = 0; i <= partition->max_label; i++) {
    VECTOR(n_new_tags_cum)[i + 1] += VECTOR(n_new_tags_cum)[i];
  }

  n_new_tags = VECTOR(n_new_tags_cum)[partition->max_label + 1];

  igraph_vector_int_init(&new_tags, n_new_tags);
  for (igraph_integer_t i = 0; i < n_new_tags; i++) {
    VECTOR(new_tags)[i] = se2_partition_new_label(partition);
  }

  igraph_integer_t current_label;
  while ((node_id = se2_iterator_next(node_iter)) != -1) {
    current_label = LABEL(*partition)[node_id];
    if (se2_partition_community_size(partition, current_label) >=
        min_community_size) {
      RELABEL(*partition)[node_id] =
        VECTOR(new_tags)[RNG_INTEGER(VECTOR(n_new_tags_cum)[current_label],
                                     VECTOR(n_new_tags_cum)[current_label + 1] - 1)];
    }
  }

  igraph_vector_int_destroy(&new_tags);
  igraph_vector_int_destroy(&n_nodes_to_move);
  igraph_vector_int_destroy(&n_new_tags_cum);
  se2_iterator_destroy(node_iter);

  se2_partition_commit_changes(partition);
}

/* For each community, find the communities that would cause the greatest
increase in modularity if merged.

merge_candidates: a vector of indices where each value is the best community to
merge with the ith community.
modularity_change: a vector of how much the modularity would change if the
corresponding merge_candidates were combined.

modularity_change is capped to be always non-negative. */
static void se2_best_merges(igraph_t const* graph,
                            igraph_vector_t const* weights,
                            se2_partition const* partition,
                            igraph_vector_int_t* merge_candidates,
                            igraph_vector_t* modularity_change,
                            igraph_integer_t const n_labels)
{
  igraph_real_t total_weight;
  igraph_eit_t eit;
  igraph_matrix_t crosstalk;
  igraph_vector_t from_edge_probability;
  igraph_vector_t to_edge_probability;

  igraph_eit_create(graph, igraph_ess_all(IGRAPH_EDGEORDER_ID), &eit);
  igraph_matrix_init(&crosstalk, n_labels, n_labels);
  igraph_vector_init(&from_edge_probability, n_labels);
  igraph_vector_init(&to_edge_probability, n_labels);

  igraph_vector_int_fill(merge_candidates, -1);

  if (weights) {
    total_weight = igraph_vector_sum(weights);
  } else {
    total_weight = igraph_ecount(graph);
  }

  igraph_integer_t eid;
  while (!IGRAPH_EIT_END(eit)) {
    eid = IGRAPH_EIT_GET(eit);
    MATRIX(crosstalk,
           LABEL(*partition)[IGRAPH_FROM(graph, eid)],
           LABEL(*partition)[IGRAPH_TO(graph, eid)]) +=
             weights ? VECTOR(*weights)[eid] : 1;
    IGRAPH_EIT_NEXT(eit);
  }

  for (igraph_integer_t i = 0; i < n_labels; i++) {
    for (igraph_integer_t j = 0; j < n_labels; j++) {
      MATRIX(crosstalk, i, j) /= total_weight;
    }
  }

  igraph_matrix_rowsum(&crosstalk, &from_edge_probability);
  igraph_matrix_colsum(&crosstalk, &to_edge_probability);

  igraph_real_t modularity_delta;
  for (igraph_integer_t i = 0; i < n_labels; i++) {
    for (igraph_integer_t j = (i + 1); j < n_labels; j++) {
      modularity_delta =
        MATRIX(crosstalk, i, j) + MATRIX(crosstalk, j, i) -
        (VECTOR(from_edge_probability)[i] * VECTOR(to_edge_probability)[j]) -
        (VECTOR(from_edge_probability)[j] * VECTOR(to_edge_probability)[i]);

      if (modularity_delta > VECTOR(*modularity_change)[i]) {
        VECTOR(*modularity_change)[i] = modularity_delta;
        VECTOR(*merge_candidates)[i] = j;
      }

      if (modularity_delta > VECTOR(*modularity_change)[j]) {
        VECTOR(*modularity_change)[j] = modularity_delta;
        VECTOR(*merge_candidates)[j] = i;
      }
    }
  }

  igraph_eit_destroy(&eit);
  igraph_matrix_destroy(&crosstalk);
  igraph_vector_destroy(&from_edge_probability);
  igraph_vector_destroy(&to_edge_probability);
}

/* Since used labels are not necessarily sequential, modularity change can be
larger than the number of labels in use. To get the median, have to find
elements of modularity change corresponding to labels in use.*/
igraph_real_t se2_modularity_median(se2_partition* partition,
                                    igraph_vector_t* modularity_change)
{
  igraph_vector_t modularity_change_without_gaps;
  se2_iterator* label_iter = se2_iterator_random_label_init(partition, 0);
  igraph_real_t res;

  igraph_vector_init(&modularity_change_without_gaps, partition->n_labels);

  igraph_integer_t label_id = 0;
  igraph_integer_t label_i = 0;
  while ((label_id = se2_iterator_next(label_iter)) != -1) {
    VECTOR(modularity_change_without_gaps)[label_i] = VECTOR(
        *modularity_change)[label_id];
    label_i++;
  }

  res = se2_vector_median(&modularity_change_without_gaps);

  igraph_vector_destroy(&modularity_change_without_gaps);
  se2_iterator_destroy(label_iter);

  return res;
}

igraph_bool_t se2_merge_well_connected_communities(igraph_t const* graph,
    igraph_vector_t const* weights,
    se2_partition* partition, igraph_real_t* max_prev_merge_threshold)
{
  igraph_integer_t max_label = se2_partition_max_label(partition);
  igraph_vector_int_t merge_candidates;
  igraph_vector_t modularity_change;
  igraph_real_t min_merge_improvement;
  igraph_real_t median_modularity_change;
  igraph_integer_t n_positive_changes = 0;
  igraph_integer_t n_merges = 0;

  igraph_vector_int_init(&merge_candidates, max_label + 1);
  igraph_vector_init(&modularity_change, max_label + 1);

  se2_best_merges(graph, weights, partition, &merge_candidates,
                  &modularity_change,
                  max_label + 1);

  for (igraph_integer_t i = 0; i <= max_label; i++) {
    if (VECTOR(modularity_change)[i] > 0) {
      n_positive_changes++;
    }
  }

  if (n_positive_changes == 0) {
    goto cleanup_early;
  }

  for (igraph_integer_t i = 0; i <= max_label; i++) {
    if (VECTOR(merge_candidates)[i] == -1) {
      continue;
    }

    VECTOR(modularity_change)[i] /=
      (se2_partition_community_size(partition, i) +
       se2_partition_community_size(partition, VECTOR(merge_candidates)[i]));
  }

  min_merge_improvement = igraph_vector_sum(&modularity_change) /
                          n_positive_changes;

  if (min_merge_improvement < (0.5 * *max_prev_merge_threshold)) {
    goto cleanup_early;
  }

  if (min_merge_improvement > *max_prev_merge_threshold) {
    *max_prev_merge_threshold = min_merge_improvement;
  }

  median_modularity_change = se2_modularity_median(partition,
                             &modularity_change);

  igraph_vector_bool_t merged_labels;
  igraph_vector_int_t sort_index;

  igraph_vector_bool_init(&merged_labels, max_label + 1);
  igraph_vector_int_init(&sort_index, max_label + 1);
  igraph_vector_qsort_ind(&modularity_change, &sort_index, IGRAPH_DESCENDING);

  if (VECTOR(modularity_change)[VECTOR(sort_index)[0]] <=
      min_merge_improvement) {
    goto cleanup_sort;
  }

  igraph_integer_t c1, c2;
  igraph_real_t delta;
  for (igraph_integer_t i = 0; i <= max_label; i++) {
    c1 = VECTOR(sort_index)[i];
    c2 = VECTOR(merge_candidates)[c1];
    delta = VECTOR(modularity_change)[c1];

    if (delta <= median_modularity_change) {
      // Since in order, as soon as one is too small all after must be too
      // small.
      break;
    }

    if ((VECTOR(merged_labels)[c1]) || (VECTOR(merged_labels)[c2])) {
      continue;
    }

    if ((se2_partition_community_size(partition, c1) < 2) ||
        (se2_partition_community_size(partition, c2) < 2)) {
      continue;
    }

    VECTOR(merged_labels)[c1] = true;
    VECTOR(merged_labels)[c2] = true;

    se2_partition_merge_labels(partition, c1, c2);
    n_merges++;
  }

  if (n_merges > 0) {
    se2_partition_commit_changes(partition);
  }

cleanup_sort:
  igraph_vector_bool_destroy(&merged_labels);
  igraph_vector_int_destroy(&sort_index);

cleanup_early:
  igraph_vector_int_destroy(&merge_candidates);
  igraph_vector_destroy(&modularity_change);

  return n_merges == 0;
}
