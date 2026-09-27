# get_freqs() rejects non-survey-base objects

    Code
      get_freqs(data.frame(x = 1:3, w = 1), x)
    Condition
      Error in `.check_unsupported_class()`:
      x `get_freqs()` requires a survey design object.
      i Got <data.frame>.

# get_freqs() rejects invalid variance argument

    Code
      get_freqs(d, group, variance = "bad_val")
    Condition
      Error in `get_freqs()`:
      x `variance` values must be from "se", "ci", "var", "cv", "moe", or "deff".
      i Unknown value: "bad_val".

# get_freqs() rejects invalid conf_level

    Code
      get_freqs(d, group, conf_level = 1.5)
    Condition
      Error in `get_freqs()`:
      x `conf_level` must be a single number strictly between 0 and 1.
      i Got 1.5.

# get_freqs() rejects invalid name_style

    Code
      get_freqs(d, group, name_style = "tidyverse")
    Condition
      Error in `get_freqs()`:
      x `name_style` must be "\"surveycore\"" or "\"broom\"".
      i Got "tidyverse".

# get_freqs() throws surveycore_error_all_na when na.rm=FALSE and all-NA

    Code
      get_freqs(d, group_all_na, na.rm = FALSE)
    Condition
      Error in `get_freqs()`:
      x All values of group_all_na are `NA`.
      i Cannot compute estimate with `na.rm = FALSE`. Set `na.rm = TRUE` to exclude `NA` values.

# get_freqs() rejects negative decimals

    Code
      get_freqs(d, group, decimals = -1L)
    Condition
      Error in `get_freqs()`:
      x `decimals` must be a non-negative whole number or `NULL`.
      i Got -1.

# get_freqs() rejects na.rm = NA with surveycore_error_na_rm_not_logical

    Code
      get_freqs(d, y3, group = grp, na.rm = NA)
    Condition
      Error in `get_freqs()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

