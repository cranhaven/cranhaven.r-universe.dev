# get_corr() throws for non-survey-base object

    Code
      get_corr(list(x = 1), x = c(y1, y2))
    Condition
      Error in `.check_unsupported_class()`:
      x `get_corr()` requires a survey design object.
      i Got <list>.

# get_corr() throws surveycore_error_insufficient_variables for < 2 vars

    Code
      get_corr(d, x = y1)
    Condition
      Error in `get_corr()`:
      x `get_corr()` requires at least 2 variables, but `x` resolved to 1 variable.

# get_corr() rejects na.rm = NA with surveycore_error_na_rm_not_logical

    Code
      get_corr(d, x = c(y1, y2), group = grp, na.rm = NA)
    Condition
      Error in `get_corr()`:
      x `na.rm` must be `TRUE` or `FALSE`.
      i Got `NA`.

# get_corr() polychoric raises PC-7 for survey_nonprob without repweights

    Code
      get_corr(d, x = c(ord1, ord2), method = "polychoric")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polychoric"` is not supported for <surveycore::survey_nonprob> designs without replicate weights.
      i Supply bootstrap replicate weights via `repweights` in `as_survey_nonprob()` to use this method.
      v Use `method = "pearson"`, or supply replicate weights.

# get_corr() polyserial raises PC-7 for survey_nonprob without repweights

    Code
      get_corr(d, x = c(ord1, y2), method = "polyserial")
    Condition
      Error in `.corr_latent_pair()`:
      x `method = "polyserial"` is not supported for <surveycore::survey_nonprob> designs without replicate weights.
      i Supply bootstrap replicate weights via `repweights` in `as_survey_nonprob()` to use this method.
      v Use `method = "pearson"`, or supply replicate weights.

