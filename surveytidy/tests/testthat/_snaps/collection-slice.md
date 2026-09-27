# slice.survey_collection raises slice_zero on integer(0) BEFORE dispatch

    Code
      dplyr::slice(coll, integer(0))
    Condition
      Error in `.check_slice_zero()`:
      x `slice()` arguments would produce 0 rows on every member of the collection.
      i Survey objects require at least 1 row, so the operation cannot proceed.
      v Pass a non-zero `n` or `prop`, or use `filter()` for domain estimation (keeps all rows).

