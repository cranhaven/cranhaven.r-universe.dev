#include "se2_print.h"
#include "se2_reweight_graph.h"

#define ABS(a) (a) > 0 ? (a) : -(a);

static igraph_real_t skewness(igraph_t const* graph,
                              igraph_vector_t const* weights)
{
  if (!weights) {
    return 0;
  }

  igraph_integer_t n_edges = igraph_ecount(graph);
  igraph_real_t avg = igraph_vector_sum(weights) / n_edges;
  igraph_real_t numerator = 0;
  igraph_real_t denominator = 0;
  igraph_real_t value = 0;
  igraph_real_t value_sq = 0;
  igraph_real_t skew = 0;

  for (igraph_integer_t i = 0; i < n_edges; i++) {
    value = VECTOR(*weights)[i] - avg;
    value_sq = value * value;
    denominator += value_sq;
    numerator += value * value_sq;
  }
  denominator = sqrt((double)denominator);
  denominator = denominator * denominator * denominator;

  skew = (numerator / n_edges) / denominator;
  skew /= sqrt((double)n_edges);

  return skew;
}

static void se2_mean_link_weight(igraph_t const* graph,
                                 igraph_vector_t const* weights,
                                 igraph_vector_t* diagonal_weights)
{
  igraph_eit_t eit;
  igraph_integer_t eid;
  igraph_vector_int_t signs;

  igraph_vector_int_init(&signs, igraph_vector_size(diagonal_weights));
  igraph_eit_create(graph, igraph_ess_all(IGRAPH_EDGEORDER_ID), &eit);
  while (!IGRAPH_EIT_END(eit)) {
    eid = IGRAPH_EIT_GET(eit);
    VECTOR(*diagonal_weights)[IGRAPH_FROM(graph, eid)] += VECTOR(*weights)[eid];
    VECTOR(signs)[IGRAPH_FROM(graph, eid)] +=
      VECTOR(*weights)[eid] < 0 ? -1 : 1;
    IGRAPH_EIT_NEXT(eit);
  }

  for (igraph_integer_t i = 0; i < igraph_vector_size(diagonal_weights); i++) {
    if (VECTOR(signs)[i] == 0) {
      continue;
    }

    VECTOR(*diagonal_weights)[i] /= VECTOR(signs)[i];
  }

  igraph_vector_int_destroy(&signs);
  igraph_eit_destroy(&eit);
}

static void se2_new_diagonal(igraph_t* graph, igraph_vector_t* weights,
                             igraph_bool_t is_skewed)
{
  igraph_integer_t n_diagonal_edges = igraph_vcount(graph);
  igraph_integer_t n_edges = igraph_ecount(graph);
  igraph_vector_int_t diagonal_edges;
  igraph_vector_t diagonal_weights;

  igraph_vector_int_init(&diagonal_edges, 2 * n_diagonal_edges);
  for (igraph_integer_t i = 0; i < (2 * n_diagonal_edges); i += 2) {
    VECTOR(diagonal_edges)[i] = i / 2;
    VECTOR(diagonal_edges)[i + 1] = i / 2;
  }
  igraph_add_edges(graph, &diagonal_edges, NULL);
  igraph_vector_int_destroy(&diagonal_edges);

  if (!weights) {
    return;
  }

  igraph_vector_resize(weights, n_edges + n_diagonal_edges);
  igraph_vector_init(&diagonal_weights, n_diagonal_edges);

  if (is_skewed) {
    se2_puts("high skew to edge weight distribution; reweighting main diag");
    se2_mean_link_weight(graph, weights, &diagonal_weights);
  } else {
    igraph_vector_fill(&diagonal_weights, 1);
  }

  for (igraph_integer_t i = 0; i < n_diagonal_edges; i++) {
    VECTOR(*weights)[i + n_edges] = VECTOR(diagonal_weights)[i];
  }

  igraph_vector_destroy(&diagonal_weights);
}

static void se2_remove_diagonal(igraph_t* graph, igraph_vector_t* weights)
{
  igraph_eit_t eit;
  igraph_integer_t n_nodes = igraph_vcount(graph);
  igraph_integer_t n_edges = igraph_ecount(graph);
  igraph_integer_t n_diagonal_edges = 0;
  igraph_vector_int_t diagonal_eids;
  igraph_es_t diagonal_es;
  igraph_integer_t eid;

  igraph_vector_int_init(&diagonal_eids, n_nodes);
  igraph_eit_create(graph, igraph_ess_all(IGRAPH_EDGEORDER_ID), &eit);

  while (!(IGRAPH_EIT_END(eit))) {
    eid = IGRAPH_EIT_GET(eit);

    if ((n_diagonal_edges > 0) && weights) {
      VECTOR(*weights)[eid - n_diagonal_edges] = VECTOR(*weights)[eid];
    }

    if (IGRAPH_FROM(graph, eid) == IGRAPH_TO(graph, eid)) {
      VECTOR(diagonal_eids)[n_diagonal_edges] = eid;
      n_diagonal_edges++;
    }

    IGRAPH_EIT_NEXT(eit);
  }

  if ((weights) && (n_diagonal_edges > 0)) {
    igraph_vector_remove_section(weights, n_edges - n_diagonal_edges, n_edges);
    igraph_vector_int_resize(&diagonal_eids, n_diagonal_edges);
    igraph_es_vector(&diagonal_es, &diagonal_eids);

    igraph_delete_edges(graph, diagonal_es);
  }

  igraph_es_destroy(&diagonal_es);
  igraph_vector_int_destroy(&diagonal_eids);
  igraph_eit_destroy(&eit);
}

static void se2_reweight_i(igraph_t* graph, igraph_vector_t* weights)
{
  if (!weights) {
    return;
  }

  igraph_real_t max_magnitude_weight = 0;
  igraph_real_t current_magnitude = 0;
  for (igraph_integer_t i = 0; i < igraph_ecount(graph); i++) {
    current_magnitude = ABS(VECTOR(*weights)[i]);
    if (current_magnitude > max_magnitude_weight) {
      max_magnitude_weight = current_magnitude;
    }
  }

  for (igraph_integer_t i = 0; i < igraph_ecount(graph); i++) {
    VECTOR(*weights)[i] /= max_magnitude_weight;
  }
}

static void se2_add_offset(igraph_t const* graph, igraph_vector_t* weights)
{
  igraph_integer_t n_nodes = igraph_vcount(graph);
  igraph_integer_t n_edges = igraph_ecount(graph);
  igraph_real_t offset = 0;

  se2_puts("adding very small offset to all edges");

  // Diagonal weights are the last n_node weights.
  for (igraph_integer_t i = (n_edges - n_nodes); i < n_edges; i++) {
    offset += VECTOR(*weights)[i];
  }
  offset /= n_nodes;

  for (igraph_integer_t i = 0; i < n_edges; i++) {
    VECTOR(*weights)[i] = ((1 - offset) * VECTOR(*weights)[i]) + offset;
  }
}

void se2_reweight(igraph_t* graph, igraph_vector_t* weights)
{
  igraph_bool_t is_skewed = skewness(graph, weights) >= 2;

  se2_reweight_i(graph, weights);
  se2_remove_diagonal(graph, weights);
  se2_new_diagonal(graph, weights, is_skewed);

  if ((is_skewed) && (igraph_vector_min(weights) >= 0)) {
    se2_add_offset(graph, weights);
  }
}
