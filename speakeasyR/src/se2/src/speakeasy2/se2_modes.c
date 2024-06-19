#include <stdlib.h>
#include <string.h>

#include "se2_modes.h"
#include "se2_label.h"

#define TYPICAL_FRACTION_NODES_TO_UPDATE 0.9
#define NURTURE_FRACTION_NODES_TO_UPDATE 0.9
#define FRACTION_NODES_TO_BUBBLE 0.9
#define POST_PEAK_BUBBLE_LIMIT 2

typedef enum {
  SE2_TYPICAL = 0,
  SE2_BUBBLE,
  SE2_MERGE,
  SE2_NURTURE,
  SE2_NUM_MODES
} se2_mode;

struct se2_tracker {
  se2_mode mode;
  igraph_integer_t* time_since_last;
  igraph_bool_t allowed_to_merge;
  igraph_real_t max_prev_merge_threshold;
  igraph_bool_t is_partition_stable;
  igraph_bool_t bubbling_has_peaked;
  igraph_integer_t smallest_community_to_bubble;
  igraph_integer_t time_since_bubbling_peaked;
  igraph_integer_t max_labels_after_bubbling;
  igraph_integer_t labels_after_last_bubbling;
  igraph_integer_t post_intervention_count;
  igraph_integer_t n_partitions;
  igraph_bool_t intervention_event;
};

se2_tracker* se2_tracker_init(se2_options const* opts)
{
  se2_tracker* tracker = malloc(sizeof(*tracker));

  igraph_integer_t* time_since_mode_tracker = calloc(SE2_NUM_MODES,
    sizeof(*time_since_mode_tracker));

  se2_tracker new_tracker = {
    .mode = SE2_TYPICAL,
    .time_since_last = time_since_mode_tracker,
    .allowed_to_merge = false,
    .max_prev_merge_threshold = 0,
    .is_partition_stable = false,
    .bubbling_has_peaked = false,
    .smallest_community_to_bubble = opts->minclust,
    .time_since_bubbling_peaked = 0,
    .max_labels_after_bubbling = 0,
    .labels_after_last_bubbling = 0,
    .post_intervention_count = -(opts->discard_transient) + 1,
    .n_partitions = opts->target_partitions,
    .intervention_event = false,
  };

  memcpy(tracker, &new_tracker, sizeof(new_tracker));

  return tracker;
}

void se2_tracker_destroy(se2_tracker* tracker)
{
  free(tracker->time_since_last);
  free(tracker);
}

igraph_integer_t se2_tracker_mode(se2_tracker const* tracker)
{
  return tracker->mode;
}

igraph_bool_t se2_do_terminate(se2_tracker* tracker)
{
  // Should never be greater than n_partitions.
  return tracker->post_intervention_count >= tracker->n_partitions;
}

igraph_bool_t se2_do_save_partition(se2_tracker* tracker)
{
  return tracker->intervention_event;
}

static void se2_select_mode(igraph_integer_t const time, se2_tracker* tracker)
{
  tracker->mode = SE2_TYPICAL; // Default

  if (time < 20) {
    return;
  }

  if (tracker->allowed_to_merge) {
    if ((tracker->time_since_last[SE2_MERGE] > 1) &&
        (tracker->time_since_last[SE2_BUBBLE] > 3)) {
      tracker->mode = SE2_MERGE;
      return;
    }
  } else {
    if ((tracker->time_since_last[SE2_MERGE] > 2) &&
        (tracker->time_since_last[SE2_BUBBLE] > 14)) {
      tracker->mode = SE2_BUBBLE;
      return;
    }

    if ((tracker->time_since_last[SE2_MERGE] > 1) &&
        (tracker->time_since_last[SE2_BUBBLE] < 5)) {
      tracker->mode = SE2_NURTURE;
      return;
    }
  }
}

static void se2_post_step_hook(se2_tracker* tracker)
{
  tracker->intervention_event = false;
  tracker->time_since_last[tracker->mode] = 0;
  for (igraph_integer_t i = 0; i < SE2_NUM_MODES; i++) {
    tracker->time_since_last[i]++;
  }

  switch (tracker->mode) {
  case SE2_BUBBLE:
    if (!tracker->bubbling_has_peaked) {
      if ((tracker->labels_after_last_bubbling > 2) &&
          (tracker->max_labels_after_bubbling >
           (tracker->labels_after_last_bubbling * 0.9))) {
        tracker->bubbling_has_peaked = true;
      }

      if (tracker->labels_after_last_bubbling >
          tracker->max_labels_after_bubbling) {
        tracker->max_labels_after_bubbling =
          tracker->labels_after_last_bubbling;
      }
    }

    if (tracker->bubbling_has_peaked) {
      tracker->time_since_bubbling_peaked++;
      if (tracker->time_since_bubbling_peaked >= POST_PEAK_BUBBLE_LIMIT) {
        tracker->time_since_bubbling_peaked = 0;
        tracker->allowed_to_merge = true;
      }
    }
    break;

  case SE2_MERGE:
    tracker->bubbling_has_peaked = false;
    tracker->time_since_bubbling_peaked = 0;
    tracker->max_labels_after_bubbling = 0;

    if (tracker->is_partition_stable) {
      tracker->allowed_to_merge = false;
      tracker->post_intervention_count++;
      if (tracker->post_intervention_count > 0) {
        tracker->intervention_event = true;
      }
    }

  default: // Just to "handle" all cases even though not needed.
    break;
  }
}

static void se2_typical_mode(igraph_t const* graph,
                             igraph_vector_t const* weights,
                             se2_partition* partition)
{
  se2_find_most_specific_labels(graph, weights, partition,
                                TYPICAL_FRACTION_NODES_TO_UPDATE);
}

static void se2_bubble_mode(igraph_t const* graph,
                            se2_partition* partition,
                            se2_tracker* tracker)
{
  se2_burst_large_communities(graph, partition, FRACTION_NODES_TO_BUBBLE,
                              tracker->smallest_community_to_bubble);
  tracker->labels_after_last_bubbling = partition->n_labels;
}

static void se2_merge_mode(igraph_t const* graph,
                           igraph_vector_t const* weights,
                           se2_partition* partition,
                           se2_tracker* tracker)
{
  tracker->is_partition_stable = se2_merge_well_connected_communities(graph,
                                 weights,
                                 partition,
                                 &(tracker->max_prev_merge_threshold));
}

static void se2_nurture_mode(igraph_t const* graph,
                             igraph_vector_t const* weights,
                             se2_partition* partition)
{
  se2_relabel_worst_nodes(graph, weights, partition,
                          NURTURE_FRACTION_NODES_TO_UPDATE);
}

void se2_mode_run_step(igraph_t const* graph,
                       igraph_vector_t const* weights,
                       se2_partition* partition, se2_tracker* tracker,
                       igraph_integer_t const time)
{
  se2_select_mode(time, tracker);

  switch (tracker->mode) {
  case SE2_TYPICAL:
    se2_typical_mode(graph, weights, partition);
    break;
  case SE2_BUBBLE:
    se2_bubble_mode(graph, partition, tracker);
    break;
  case SE2_MERGE:
    se2_merge_mode(graph, weights, partition, tracker);
    break;
  case SE2_NURTURE:
    se2_nurture_mode(graph, weights, partition);
    break;
  case SE2_NUM_MODES:
    // Never occurs.
    break;
  }

  se2_post_step_hook(tracker);
}
