# get_covariance() rejects non-survey design

    Code
      get_covariance(df, c(x, y))
    Condition
      Error in `.check_unsupported_class()`:
      x `get_covariance()` requires a survey design object.
      i Got <data.frame>.

# get_covariance() rejects empty selection (0 cols)

    Code
      get_covariance(sc, dplyr::any_of("nonexistent_var"))
    Condition
      Error in `get_covariance()`:
      x `get_covariance()` requires at least 2 variables, but `x` resolved to 0 variables.

# get_covariance() rejects single-variable selection (1 col)

    Code
      get_covariance(sc, c(y1))
    Condition
      Error in `get_covariance()`:
      x `get_covariance()` requires at least 2 variables, but `x` resolved to 1 variable.

# get_covariance() rejects invalid variance value

    Code
      get_covariance(sc, c(y1, y2), variance = "foo")
    Condition
      Error in `get_covariance()`:
      x `variance` values must be from "se", "ci", "var", "cv", "moe", or "deff".
      i Unknown value: "foo".

# get_covariance() rejects invalid conf_level

    Code
      get_covariance(sc, c(y1, y2), conf_level = 0)
    Condition
      Error in `get_covariance()`:
      x `conf_level` must be a single number strictly between 0 and 1.
      i Got 0.

# get_covariance() rejects invalid decimals

    Code
      get_covariance(sc, c(y1, y2), decimals = -1)
    Condition
      Error in `get_covariance()`:
      x `decimals` must be a non-negative whole number or `NULL`.
      i Got -1.

# get_covariance() rejects invalid name_style

    Code
      get_covariance(sc, c(y1, y2), name_style = "foo")
    Condition
      Error in `get_covariance()`:
      x `name_style` must be "\"surveycore\"" or "\"broom\"".
      i Got "foo".

# get_covariance() rejects non-logical na.rm

    Code
      get_covariance(sc, c(y1, y2), na.rm = NA)
    Condition
      Error in `get_covariance()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

# get_covariance() fires small_cell when any pair has n < min_cell_n

    Code
      suppressWarnings(withCallingHandlers(get_covariance(sc, c(y1, y2), min_cell_n = nrow(
        df) + 1L), warning = function(w) {
        if (inherits(w, "surveycore_warning_small_cell")) {
          message(conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }))
    Message
      ! 1 cell has fewer than 201 unweighted observations. Estimates in these cells may be unreliable for public reporting (AAPOR guidance).
    Output
      # A tibble: 1 x 6
        var1  var2  covariance ci_low ci_high     n
        <fct> <fct>      <dbl>  <dbl>   <dbl> <int>
      1 y1    y2        0.0661  -1.20    1.33   200

# get_covariance() fires covariance_non_numeric when dropping non-numeric vars

    Code
      suppressWarnings(withCallingHandlers(get_covariance(sc, c(y1, y2, group)),
      warning = function(w) {
        if (inherits(w, "surveycore_warning_covariance_non_numeric")) {
          message(conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }))
    Message
      ! Dropped non-numeric variable from `x`: group.
      i `get_covariance()` requires numeric variables.
    Output
      # A tibble: 1 x 6
        var1  var2  covariance ci_low ci_high     n
        <fct> <fct>      <dbl>  <dbl>   <dbl> <int>
      1 y1    y2         0.269  -2.29    2.83   100

# get_covariance() fires covariance_all_na when pair is all-NA in domain

    Code
      suppressWarnings(withCallingHandlers(get_covariance(sc, c(y1, allna)), warning = function(
        w) {
        if (inherits(w, "surveycore_warning_covariance_all_na")) {
          message(conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }))
    Message
      ! Pair (y1, allna) is all-`NA` on the active domain. Returning `NaN` with `n = 0`.
    Output
      # A tibble: 1 x 6
        var1  var2  covariance ci_low ci_high     n
        <fct> <fct>      <dbl>  <dbl>   <dbl> <int>
      1 y1    allna        NaN    NaN     NaN     0

# get_covariance() fires covariance_insufficient_n when n == 1

    Code
      suppressWarnings(withCallingHandlers(get_covariance(sc, c(y1, one)), warning = function(
        w) {
        if (inherits(w, "surveycore_warning_covariance_insufficient_n")) {
          message(conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }))
    Message
      ! Pair (y1, one) has 1 pairwise-complete observation in the active domain; covariance requires at least 2. Returning `NaN`.
    Output
      # A tibble: 1 x 6
        var1  var2  covariance ci_low ci_high     n
        <fct> <fct>      <dbl>  <dbl>   <dbl> <int>
      1 y1    one          NaN    NaN     NaN     1

# get_covariance() collection: .if_missing_var='error' aborts when var missing

    Code
      get_covariance(coll, c(focal, y1), .if_missing_var = "error")
    Condition
      Error in `.dispatch_over_collection()`:
      x Survey "w2" in the collection is missing a required variable.
      i Original error: x Variable "focal" not found in survey data. i Available: "psu", "strata", "fpc", "wt", "y1", "y2", "y3", and "group".
      v Set `.if_missing_var = "skip"` to drop surveys missing the variable.
      Caused by error in `value[[3L]]()`:
      x Variable "focal" not found in survey data.
      i Available: "psu", "strata", "fpc", "wt", "y1", "y2", "y3", and "group".

# get_covariance() collection: .if_missing_var='skip' with all missing aborts

    Code
      get_covariance(coll, c(focal, y1), .if_missing_var = "skip")
    Message
      i Skipped 2 surveys missing the requested variable: "w1" and "w2".
    Condition
      Error in `.dispatch_over_collection()`:
      x No surveys in the collection contained the requested variable.

# get_covariance() collection: .id collision aborts

    Code
      get_covariance(coll, c(y1, y2), .id = "covariance")
    Condition
      Error in `.dispatch_over_collection()`:
      x `.id` value "covariance" conflicts with a column produced by the analysis function.
      v Pass a different `.id`, e.g. `.id = "wave"`.

