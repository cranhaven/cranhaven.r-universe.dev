# get_variance() rejects non-survey design with unsupported_class

    Code
      get_variance(df, x)
    Condition
      Error in `.check_unsupported_class()`:
      x `get_variance()` requires a survey design object.
      i Got <data.frame>.

# get_variance() rejects empty variable selection with wrong_variable_count

    Code
      get_variance(sc, dplyr::any_of("nonexistent_var"))
    Condition
      Error in `get_variance()`:
      x `get_variance()` requires at least one variable.
      i `x` resolved to 0 variables.

# get_variance() rejects factor variable as non_numeric_variable

    Code
      get_variance(sc, f)
    Condition
      Error in `get_variance()`:
      x `x` must be numeric, not <factor>.
      i Column f cannot be used with `get_variance()`.

# get_variance() rejects character variable as non_numeric_variable

    Code
      get_variance(sc, ch)
    Condition
      Error in `get_variance()`:
      x `x` must be numeric, not <character>.
      i Column ch cannot be used with `get_variance()`.

# get_variance() rejects invalid variance value

    Code
      get_variance(sc, y1, variance = "foo")
    Condition
      Error in `get_variance()`:
      x `variance` values must be from "se", "ci", "var", "cv", "moe", or "deff".
      i Unknown value: "foo".

# get_variance() rejects invalid conf_level

    Code
      get_variance(sc, y1, conf_level = 0)
    Condition
      Error in `get_variance()`:
      x `conf_level` must be a single number strictly between 0 and 1.
      i Got 0.

# get_variance() rejects invalid decimals

    Code
      get_variance(sc, y1, decimals = -1)
    Condition
      Error in `get_variance()`:
      x `decimals` must be a non-negative whole number or `NULL`.
      i Got -1.

# get_variance() rejects invalid name_style

    Code
      get_variance(sc, y1, name_style = "foo")
    Condition
      Error in `get_variance()`:
      x `name_style` must be "\"surveycore\"" or "\"broom\"".
      i Got "foo".

# get_variance() rejects non-logical na.rm

    Code
      get_variance(sc, y1, na.rm = NA)
    Condition
      Error in `get_variance()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

