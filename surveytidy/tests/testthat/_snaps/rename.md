# rename() warns when renaming a design variable

    Code
      invisible(rename(d, weight = wt))
    Condition
      Warning:
      ! Renamed design variable wt.
      i The survey design has been updated to track the new name.

# rename_with() errors when .fn returns non-character output

    Code
      rename_with(d, function(x) seq_along(x), .cols = starts_with("y"))
    Condition
      Error in `rename_with()`:
      x `.fn` must return a character vector.
      i Got <integer> of length 3.
      v Check that `.fn` returns a plain character vector and handles all column names uniformly.

# rename_with() errors when .fn returns wrong-length vector

    Code
      rename_with(d, function(x) x[[1L]], .cols = starts_with("y"))
    Condition
      Error in `rename_with()`:
      x `.fn` must return a character vector of the same length as its input.
      i Input had 3 names; `.fn` returned 1.
      v Check that `.fn` returns a plain character vector and handles all column names uniformly.

# rename_with() errors when .fn returns duplicate names

    Code
      rename_with(d, function(x) rep("Y1", length(x)), .cols = starts_with("y"))
    Condition
      Error in `rename_with()`:
      x `.fn` must return a character vector with no duplicate names.
      i Duplicate name: Y1.
      v Check that `.fn` returns a plain character vector and handles all column names uniformly.

# rename_with() errors when .fn returns name conflicting with existing column

    Code
      rename_with(d, function(x) "y2", .cols = dplyr::all_of("y1"))
    Condition
      Error in `rename_with()`:
      x `.fn` returned name that conflict with existing columns.
      i Conflicting name: y2.
      v Check that `.fn` returns a plain character vector and handles all column names uniformly.

