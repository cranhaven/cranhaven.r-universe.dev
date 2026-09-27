# dplyr_reconstruct() errors when a design variable is removed

    Code
      dplyr_reconstruct(bad, d)
    Condition
      Error in `dplyr_reconstruct_dispatch()`:
      x Required design variable(s) removed: wt.
      i Design variables cannot be removed from a survey object.
      v Use `select()` to hide columns without removing them.

