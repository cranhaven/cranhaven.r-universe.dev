# left_join.survey_collection raises verb_unsupported

    Code
      dplyr::left_join(coll, y, by = "group")
    Condition
      Error in `dplyr::left_join()`:
      x `left_join()` on a <survey_collection> is not supported.
      i The semantics (apply to each survey? broadcast across all?) are still being designed.
      v Apply the join inside a per-survey pipeline before constructing the collection.

# right_join.survey_collection raises verb_unsupported

    Code
      dplyr::right_join(coll, y, by = "group")
    Condition
      Error in `dplyr::right_join()`:
      x `right_join()` on a <survey_collection> is not supported.
      i The semantics (apply to each survey? broadcast across all?) are still being designed.
      v Apply the join inside a per-survey pipeline before constructing the collection.

# inner_join.survey_collection raises verb_unsupported

    Code
      dplyr::inner_join(coll, y, by = "group")
    Condition
      Error in `dplyr::inner_join()`:
      x `inner_join()` on a <survey_collection> is not supported.
      i The semantics (apply to each survey? broadcast across all?) are still being designed.
      v Apply the join inside a per-survey pipeline before constructing the collection.

# full_join.survey_collection raises verb_unsupported

    Code
      dplyr::full_join(coll, y, by = "group")
    Condition
      Error in `dplyr::full_join()`:
      x `full_join()` on a <survey_collection> is not supported.
      i The semantics (apply to each survey? broadcast across all?) are still being designed.
      v Apply the join inside a per-survey pipeline before constructing the collection.

# semi_join.survey_collection raises verb_unsupported

    Code
      dplyr::semi_join(coll, y, by = "group")
    Condition
      Error in `dplyr::semi_join()`:
      x `semi_join()` on a <survey_collection> is not supported.
      i The semantics (apply to each survey? broadcast across all?) are still being designed.
      v Apply the join inside a per-survey pipeline before constructing the collection.

# anti_join.survey_collection raises verb_unsupported

    Code
      dplyr::anti_join(coll, y, by = "group")
    Condition
      Error in `dplyr::anti_join()`:
      x `anti_join()` on a <survey_collection> is not supported.
      i The semantics (apply to each survey? broadcast across all?) are still being designed.
      v Apply the join inside a per-survey pipeline before constructing the collection.

