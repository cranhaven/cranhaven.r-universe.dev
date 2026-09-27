# .validate_shared_args() rejects unknown variance values

    Code
      .validate_shared_args("bogus", 0.95, "surveycore")
    Condition
      Error:
      x `variance` values must be from "se", "ci", "var", "cv", "moe", or "deff".
      i Unknown value: "bogus".

# .validate_shared_args() rejects non-numeric conf_level

    Code
      .validate_shared_args(NULL, "high", "surveycore")
    Condition
      Error:
      x `conf_level` must be a single number strictly between 0 and 1.
      i Got "high".

# .validate_shared_args() rejects invalid name_style

    Code
      .validate_shared_args(NULL, 0.95, "tidy")
    Condition
      Error:
      x `name_style` must be "\"surveycore\"" or "\"broom\"".
      i Got "tidy".

# .build_meta() fallback throws surveycore_error_unsupported_class

    Code
      .build_meta(list(data = data.frame(x = 1)), list())
    Condition
      Error in `.build_meta()`:
      x Unrecognized design class <list>.

# .check_unsupported_class() throws for a plain data frame

    Code
      .check_unsupported_class(data.frame(x = 1), "get_means")
    Condition
      Error in `.check_unsupported_class()`:
      x `get_means()` requires a survey design object.
      i Got <data.frame>.

# .validate_shared_args() rejects negative decimals

    Code
      .validate_shared_args(NULL, 0.95, "surveycore", decimals = -1L)
    Condition
      Error:
      x `decimals` must be a non-negative whole number or `NULL`.
      i Got -1.

# .validate_shared_args() rejects na.rm = NA with typed error

    Code
      .validate_shared_args(NULL, 0.95, "surveycore", na.rm = NA)
    Condition
      Error:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

# .nonprob_rep_na_warn() domain-NA warning does not say 'bootstrap' for JK1

    Code
      withCallingHandlers(get_means(d, y, group = grp),
      surveycore_warning_domain_replicates_na = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    Message
      ! 3 of 20 replicates have no observations in this domain (15% of R).
      i Standard errors for this cell understate variance because the scale factor `0.95` was computed for 20 replicates but only 17 contribute.
      i Consider collapsing small domain categories or increasing R in `surveywts::create_bootstrap_weights()`.
    Condition
      Warning:
      ! 2 cells have fewer than 30 unweighted observations. Estimates in these cells may be unreliable for public reporting (AAPOR guidance).
    Output
      # A tibble: 2 x 5
        grp     mean ci_low ci_high     n
        <chr>  <dbl>  <dbl>   <dbl> <int>
      1 A     -0.121 -0.427   0.186    25
      2 B     -0.168 -0.168  -0.168    25

