# empty-result error reports stored .if_missing_var source

    Code
      .dispatch_verb_over_collection(fn = dplyr::filter, verb_name = "filter",
      collection = coll, ghost_col_xyz > 0, .detect_missing = "pre_check",
      .may_change_groups = FALSE)
    Message
      i `filter()` skipped survey "m1", "m2", and "m3" (missing referenced variables).
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `filter()` produced an empty <survey_collection>.
      i All surveys were skipped because they were missing referenced variables.
      i `.if_missing_var` resolved to "skip" from the collection's stored property.
      v Inspect `names()` on the input collection and verify each member has the referenced columns.

# empty-result error reports per-call .if_missing_var source

    Code
      .dispatch_verb_over_collection(fn = dplyr::filter, verb_name = "filter",
      collection = coll, ghost_col_xyz > 0, .if_missing_var = "skip",
      .detect_missing = "pre_check", .may_change_groups = FALSE)
    Message
      i `filter()` skipped survey "m1", "m2", and "m3" (missing referenced variables).
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `filter()` produced an empty <survey_collection>.
      i All surveys were skipped because they were missing referenced variables.
      i `.if_missing_var = "skip"` was passed to this call.
      v Inspect `names()` on the input collection and verify each member has the referenced columns.

# skip path emits the typed message naming every skipped survey

    Code
      out <- .dispatch_verb_over_collection(fn = dplyr::filter, verb_name = "filter",
      collection = coll, y1 > 0, .detect_missing = "pre_check", .may_change_groups = FALSE)
    Message
      i `filter()` skipped survey "m3" (missing referenced variable).

