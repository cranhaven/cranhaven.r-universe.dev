# get_means() errors for non-numeric variable

    Code
      get_means(d, y)
    Condition
      Error in `get_means()`:
      x `x` must be numeric, not <character>.
      i Column y cannot be used with `get_means()`.

# get_means() errors for invalid variance value

    Code
      get_means(d, y, variance = "bogus")
    Condition
      Error in `get_means()`:
      x `variance` values must be from "se", "ci", "var", "cv", "moe", or "deff".
      i Unknown value: "bogus".

# get_means() rejects na.rm = NA with surveycore_error_na_rm_not_logical

    Code
      get_means(d, y1, group = grp, na.rm = NA)
    Condition
      Error in `get_means()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

