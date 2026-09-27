# slice() errors on 0-row result

    Code
      suppressWarnings(slice(d, integer(0)))
    Condition
      Error in `slice()`:
      x `slice()` produced 0 rows.
      i Survey objects require at least 1 row.
      v Use `filter()` for domain estimation (keeps all rows).

