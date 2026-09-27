# mutate.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::mutate(coll, z = region)
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `mutate()` failed on survey "m1": missing referenced variable region.
      Caused by error:
      ! Survey 'm1' is missing referenced variable: region

# mutate.survey_collection rejects .by

    Code
      dplyr::mutate(coll, z = y1 + 1, .by = "group")
    Condition
      Error in `dplyr::mutate()`:
      x `.by` is not supported on <survey_collection>.
      i Per-call grouping overrides do not compose cleanly with `coll@groups`.
      v Use `group_by()` on the collection (or set `coll@groups`) instead.

# mutate.survey_collection warns once on rowwise mixed state

    Code
      withCallingHandlers(dplyr::mutate(coll, z = y1 + 1),
      surveytidy_warning_collection_rowwise_mixed = function(cnd) {
        message(conditionMessage(cnd))
        rlang::cnd_muffle(cnd)
      })
    Message
      ! `mutate()` called on a <survey_collection> with mixed rowwise state.
      i Rowwise: "taylor"; non-rowwise: "replicate" and "twophase". Each member will be mutated under its own semantics, which may give inconsistent results.
      i Call `rowwise(coll)` or `ungroup(coll)` on the collection first to make rowwise state uniform.
      A <survey_collection> with 3 surveys:
      id: ".survey"
      if_missing_var: "error"
      "taylor": survey_taylor, 100 rows, 9 variables
      "replicate": survey_replicate, 100 rows, 14 variables
      "twophase": survey_twophase, 100 rows, 10 variables
    Code
      invisible()

