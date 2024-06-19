#include <string.h>
#include <stdlib.h>

#include "se2_print.h"
#include "se2_partitions.h"
#include "se2_random.h"

#define MAX(a, b) (a) > (b) ? (a) : (b)

struct se2_iterator {
  igraph_vector_int_t* ids;
  igraph_integer_t pos;
  igraph_integer_t n_total;
  igraph_integer_t n_iter;
  igraph_bool_t owns_ids;
};

static igraph_integer_t se2_count_labels(igraph_vector_int_t* membership,
    igraph_vector_int_t* community_sizes)
{
  igraph_integer_t max_label = igraph_vector_int_max(membership);
  igraph_integer_t n_labels = 0;
  igraph_integer_t n_nodes = igraph_vector_int_size(membership);

  igraph_vector_int_resize(community_sizes, max_label + 1);
  igraph_vector_int_null(community_sizes);
  for (igraph_integer_t i = 0; i < n_nodes; i++) {
    VECTOR(*community_sizes)[VECTOR(*membership)[i]]++;
  }

  for (igraph_integer_t i = 0; i <= max_label; i++) {
    n_labels += VECTOR(*community_sizes)[i] > 0;
  }

  return n_labels;
}

se2_partition* se2_partition_init(igraph_t const* graph,
                                  igraph_vector_int_t* initial_labels)
{
  se2_partition* partition = malloc(sizeof(*partition));
  igraph_integer_t n_nodes = igraph_vcount(graph);
  igraph_vector_int_t* reference = malloc(sizeof(*reference));
  igraph_vector_int_t* stage = malloc(sizeof(*stage));
  igraph_vector_t* specificity = malloc(sizeof(*specificity));
  igraph_vector_int_t* community_sizes = malloc(sizeof(*community_sizes));
  igraph_integer_t n_labels = 0;

  if (igraph_vector_int_size(initial_labels) != n_nodes) {
    se2_printf("Membership vector size differs from number of vertices.");
    return NULL;
  }

  igraph_vector_int_init(reference, n_nodes);
  igraph_vector_int_init(stage, n_nodes);
  igraph_vector_init(specificity, n_nodes);
  igraph_vector_int_init(community_sizes, 0);

  igraph_vector_int_update(reference, initial_labels);
  igraph_vector_int_update(stage, initial_labels);

  n_labels = se2_count_labels(initial_labels, community_sizes);

  se2_partition new_partition = {
    .n_nodes = n_nodes,
    .reference = reference,
    .stage = stage,
    .label_quality = specificity,
    .community_sizes = community_sizes,
    .n_labels = n_labels,
    .max_label = igraph_vector_int_size(community_sizes) - 1,
  };

  memcpy(partition, &new_partition, sizeof(new_partition));
  return partition;
}

void se2_partition_destroy(se2_partition* partition)
{
  igraph_vector_int_destroy(partition->reference);
  igraph_vector_int_destroy(partition->stage);
  igraph_vector_destroy(partition->label_quality);
  igraph_vector_int_destroy(partition->community_sizes);
  free(partition->reference);
  free(partition->stage);
  free(partition->label_quality);
  free(partition->community_sizes);
  free(partition);
}

void se2_iterator_shuffle(se2_iterator* iterator)
{
  iterator->pos = 0;
  se2_randperm(iterator->ids, iterator->n_total,
               iterator->n_iter);
}

void se2_iterator_reset(se2_iterator* iterator)
{
  iterator->pos = 0;
}

// WARNING: Iterator does not take ownership of the id vector so it must still
// be cleaned up by the caller.
se2_iterator* se2_iterator_from_vector(igraph_vector_int_t* ids,
                                       igraph_integer_t const n_iter)
{
  igraph_integer_t n = igraph_vector_int_size(ids);
  se2_iterator* iterator = malloc(sizeof(*iterator));
  se2_iterator new_iterator = {
    .ids = ids,
    .n_total = n,
    .n_iter = n_iter,
    .pos = 0,
    .owns_ids = false
  };

  memcpy(iterator, &new_iterator, sizeof(new_iterator));
  return iterator;
}

se2_iterator* se2_iterator_random_node_init(se2_partition const* partition,
    igraph_real_t const proportion)
{
  igraph_integer_t n_total = partition->n_nodes;
  igraph_integer_t n_iter = n_total;
  igraph_vector_int_t* nodes = malloc(sizeof(*nodes));

  igraph_vector_int_init(nodes, n_total);
  for (igraph_integer_t i = 0; i < n_total; i++) {
    VECTOR(*nodes)[i] = i;
  }

  if (proportion) {
    n_iter = n_total * proportion;
  }

  se2_iterator* iterator = se2_iterator_from_vector(nodes, n_iter);
  iterator->owns_ids = true;
  se2_iterator_shuffle(iterator);

  return iterator;
}

se2_iterator* se2_iterator_random_label_init(se2_partition const* partition,
    igraph_real_t const proportion)
{
  igraph_integer_t n_total = partition->n_labels;
  igraph_integer_t n_iter = n_total;
  igraph_vector_int_t* labels = malloc(sizeof(*labels));

  igraph_vector_int_init(labels, n_total);
  for (igraph_integer_t i = 0, j = 0; i < n_total; j++) {
    if (VECTOR(*(partition->community_sizes))[j] > 0) {
      VECTOR(*labels)[i] = j;
      i++;
    }
  }

  if (proportion) {
    n_iter = n_total * proportion;
  }

  se2_iterator* iterator = se2_iterator_from_vector(labels, n_iter);
  iterator->owns_ids = true;
  se2_iterator_shuffle(iterator);

  return iterator;
}

se2_iterator* se2_iterator_k_worst_fit_nodes_init(
  se2_partition const* partition, igraph_integer_t const k)
{
  igraph_vector_int_t* ids = malloc(sizeof(*ids));
  igraph_vector_int_init(ids, partition->n_nodes);

  igraph_vector_qsort_ind(partition->label_quality, ids, IGRAPH_ASCENDING);
  igraph_vector_int_resize(ids, k);

  se2_iterator* iterator = se2_iterator_from_vector(ids, k);
  iterator->owns_ids = true;
  se2_iterator_shuffle(iterator);

  return iterator;
}

void se2_iterator_destroy(se2_iterator* iterator)
{
  if (iterator->owns_ids) {
    igraph_vector_int_destroy(iterator->ids);
    free(iterator->ids);
  }
  free(iterator);
}

igraph_integer_t se2_iterator_next(se2_iterator* iterator)
{
  igraph_integer_t n = 0;
  if (iterator->pos == iterator->n_iter) {
    iterator->pos = 0;
    return -1;
  }

  n = VECTOR(*iterator->ids)[iterator->pos];
  iterator->pos++;

  return n;
}

igraph_integer_t se2_partition_n_nodes(se2_partition const* partition)
{
  return partition->n_nodes;
}

igraph_integer_t se2_partition_n_labels(se2_partition const* partition)
{
  return partition->n_labels;
}

igraph_integer_t se2_partition_max_label(se2_partition const* partition)
{
  return partition->max_label;
}

void se2_partition_add_to_stage(se2_partition* partition,
                                igraph_integer_t const node_id,
                                igraph_integer_t const label,
                                igraph_real_t specificity)
{
  VECTOR(*partition->stage)[node_id] = label;
  VECTOR(*partition->label_quality)[node_id] = specificity;
}

// Return an unused label.
igraph_integer_t se2_partition_new_label(se2_partition* partition)
{
  igraph_integer_t pool_size = igraph_vector_int_size(
                                 partition->community_sizes);
  igraph_integer_t next_label = 0;
  while ((VECTOR(*partition->community_sizes)[next_label]) &&
         (next_label < pool_size)) {
    next_label++;
  }

  if (next_label == igraph_vector_int_capacity(partition->community_sizes)) {
    igraph_vector_int_reserve(partition->community_sizes,
                              MAX(2 * pool_size, partition->n_nodes));
  }

  if (next_label == pool_size) {
    igraph_vector_int_push_back(partition->community_sizes, 0);
  }

  if (next_label > partition->max_label) {
    partition->max_label = next_label;
  }

  partition->n_labels++;

  // Mark new label as reserved.
  VECTOR(*partition->community_sizes)[next_label] = -1;

  return next_label;
}

static inline void se2_partition_free_label(se2_partition* partition,
    igraph_integer_t const label)
{
  VECTOR(*partition->community_sizes)[label] = 0;
  if (label == partition->max_label) {
    while ((!VECTOR(*partition->community_sizes)[partition->max_label]) &&
           (partition->max_label > 0)) {
      partition->max_label--;
    }
  }

  partition->n_labels--;
}

igraph_integer_t se2_partition_community_size(se2_partition const* partition,
    igraph_integer_t const label)
{
  return VECTOR(*partition->community_sizes)[label];
}

igraph_real_t se2_vector_median(igraph_vector_t const* vec)
{
  igraph_vector_int_t ids;
  igraph_integer_t len = igraph_vector_size(vec) - 1;
  igraph_integer_t k = len / 2;
  igraph_real_t res;

  igraph_vector_int_init(&ids, len);
  igraph_vector_qsort_ind(vec, &ids, IGRAPH_ASCENDING);
  res = VECTOR(*vec)[VECTOR(ids)[k]];

  if (len % 2) {
    res += VECTOR(*vec)[VECTOR(ids)[k + 1]];
    res /= 2;
  }

  igraph_vector_int_destroy(&ids);

  return res;
}
igraph_real_t se2_vector_int_median(igraph_vector_int_t const* vec)
{
  igraph_vector_int_t ids;
  igraph_integer_t len = igraph_vector_int_size(vec) - 1;
  igraph_integer_t k = len / 2;
  igraph_real_t res;

  igraph_vector_int_init(&ids, len);
  igraph_vector_int_qsort_ind(vec, &ids, IGRAPH_ASCENDING);
  res = VECTOR(*vec)[VECTOR(ids)[k]];

  if (len % 2) {
    res += VECTOR(*vec)[VECTOR(ids)[k + 1]];
    res /= 2;
  }

  igraph_vector_int_destroy(&ids);

  return res;
}

igraph_integer_t se2_partition_median_community_size(se2_partition const
    *partition)
{
  if (partition->n_labels == 1) {
    return partition->n_nodes;
  }

  igraph_vector_int_t community_sizes;
  se2_iterator* label_iter = se2_iterator_random_label_init(partition, 0);
  igraph_integer_t res = 0;

  igraph_vector_int_init(&community_sizes, partition->n_labels);

  igraph_integer_t label_id;
  igraph_integer_t label_i = 0;
  while ((label_id = se2_iterator_next(label_iter)) != -1) {
    VECTOR(community_sizes)[label_i] =
      (igraph_real_t)se2_partition_community_size(partition, label_id);
    label_i++;
  }
  igraph_vector_int_resize(&community_sizes, label_i);

  res = se2_vector_int_median(&community_sizes);

  se2_iterator_destroy(label_iter);
  igraph_vector_int_destroy(&community_sizes);

  return res;
}

void se2_partition_merge_labels(se2_partition* partition, igraph_integer_t c1,
                                igraph_integer_t c2)
{
  // Ensure smaller community engulfed by larger community. Not necessary.
  if (se2_partition_community_size(partition, c2) >
      se2_partition_community_size(partition, c1)) {
    igraph_integer_t swp = c1;
    c1 = c2;
    c2 = swp;
  }

  for (igraph_integer_t i = 0; i < partition->n_nodes; i++) {
    if (VECTOR(*partition->stage)[i] == c2) {
      VECTOR(*partition->stage)[i] = c1;
    }
  }

  se2_partition_free_label(partition, c2);
}

// Move nodes in mask to new label.
void se2_partition_relabel_mask(se2_partition* partition,
                                igraph_vector_bool_t const* mask)
{
  igraph_integer_t label = se2_partition_new_label(partition);
  for (igraph_integer_t i = 0; i < partition->n_nodes; i++) {
    if (VECTOR(*mask)[i]) {
      VECTOR(*partition->stage)[i] = label;
    }
  }
}

static void se2_partition_recount_community_sizes(se2_partition* partition)
{
  partition->n_labels = se2_count_labels(partition->reference,
                                         partition->community_sizes);
  partition->max_label = igraph_vector_int_size(partition->community_sizes) -
                         1;
}

void se2_partition_commit_changes(se2_partition* partition)
{
  igraph_vector_int_update(partition->reference, partition->stage);
  se2_partition_recount_community_sizes(partition);
}

void se2_reindex_membership(igraph_vector_int_t* membership)
{
  igraph_vector_int_t indices;
  igraph_integer_t n_nodes = igraph_vector_int_size(membership);

  igraph_vector_int_init(&indices, n_nodes);

  igraph_vector_int_qsort_ind(membership, &indices, IGRAPH_ASCENDING);

  igraph_integer_t c_old, c_new = -1, c_prev_node = -1;
  for (igraph_integer_t i = 0; i < n_nodes; i++) {
    c_old = VECTOR(*membership)[VECTOR(indices)[i]];
    if (c_old != c_prev_node) {
      c_new++;
      c_prev_node = c_old;
    }
    VECTOR(*membership)[VECTOR(indices)[i]] = c_new;
  }

  igraph_vector_int_destroy(&indices);
}

/* Save the state of the current working partition's committed changes to the
partition store.

NOTE: This saves only the membership ids for each node so it goes from a
se2_partition to an igraph vector despite both arguments being
"partitions". */
void se2_partition_store(se2_partition const* working_partition,
                         igraph_vector_int_list_t* partition_store,
                         igraph_integer_t const idx)
{
  igraph_vector_int_t* partition_state = igraph_vector_int_list_get_ptr(
      partition_store, idx);
  igraph_vector_int_update(partition_state, working_partition->reference);
  se2_reindex_membership(partition_state);
}
