# get_ratios() errors for non-numeric numerator

    Code
      get_ratios(d, y, x)
    Condition
      Error in `get_ratios()`:
      x `numerator` must be numeric, not <character>.
      i Column y cannot be used with `get_ratios()`.

# get_ratios() errors for non-numeric denominator

    Code
      get_ratios(d, y, x)
    Condition
      Error in `get_ratios()`:
      x `denominator` must be numeric, not <character>.
      i Column x cannot be used with `get_ratios()`.

# get_ratios() errors when all denominator values are zero

    Code
      get_ratios(d, y, x)
    Condition
      Error in `get_ratios()`:
      x All values of the denominator (x) are zero. Cannot compute ratio.
      i Provide a denominator variable with at least one non-zero value.

# get_ratios() errors for invalid variance value

    Code
      get_ratios(d, y, x, variance = "bogus")
    Condition
      Error in `get_ratios()`:
      x `variance` values must be from "se", "ci", "var", "cv", "moe", or "deff".
      i Unknown value: "bogus".

# get_ratios() rejects na.rm = NA with surveycore_error_na_rm_not_logical

    Code
      get_ratios(d, y1, y2, group = grp, na.rm = NA)
    Condition
      Error in `get_ratios()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

