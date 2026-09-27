# filter.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::filter(coll, region == "north")
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `filter()` failed on survey "m1": missing referenced variable region.
      Caused by error:
      ! Survey 'm1' is missing referenced variable: region

# filter.survey_collection raises emptied error when all members skipped

    Code
      dplyr::filter(coll_skip, ghost_col_xyz > 0)
    Message
      i `filter()` skipped survey "m1", "m2", and "m3" (missing referenced variables).
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `filter()` produced an empty <survey_collection>.
      i All surveys were skipped because they were missing referenced variables.
      i `.if_missing_var` resolved to "skip" from the collection's stored property.
      v Inspect `names()` on the input collection and verify each member has the referenced columns.

# filter.survey_collection rejects .by

    Code
      dplyr::filter(coll, y1 > 0, .by = "group")
    Condition
      Error in `dplyr::filter()`:
      x `.by` is not supported on <survey_collection>.
      i Per-call grouping overrides do not compose cleanly with `coll@groups`.
      v Use `group_by()` on the collection (or set `coll@groups`) instead.

