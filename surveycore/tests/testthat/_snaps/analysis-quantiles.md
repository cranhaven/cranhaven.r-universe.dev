# get_quantiles() rejects non-survey-base input

    Code
      get_quantiles(list(x = 1), y1)
    Condition
      Error in `.check_unsupported_class()`:
      x `get_quantiles()` requires a survey design object.
      i Got <list>.

# get_quantiles() rejects probs outside (0,1)

    Code
      get_quantiles(d, y1, probs = c(0.25, 1.1))
    Condition
      Error in `get_quantiles()`:
      x `probs` must be a non-empty numeric vector with all values in (0, 1).
      i Invalid value: 1.1.

# get_quantiles() rejects non-numeric x

    Code
      get_quantiles(d, char_col, probs = 0.5)
    Condition
      Error in `get_quantiles()`:
      x `x` must be numeric, not <character>.
      i Column char_col cannot be used with `get_quantiles()`.

# get_quantiles() rejects multiple x variables

    Code
      get_quantiles(d, c(y1, y2), probs = 0.5)
    Condition
      Error in `get_quantiles()`:
      x `get_quantiles()` requires exactly one variable.
      i `x` resolved to 2 variables.

# get_quantiles() rejects na.rm = NA with surveycore_error_na_rm_not_logical

    Code
      get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = NA)
    Condition
      Error in `get_quantiles()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

