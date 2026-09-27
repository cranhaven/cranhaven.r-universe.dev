# .validate_rscales() errors for NA values via as_survey_nonprob()

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), rscales = c(1,
        NA_real_))
    Condition
      Error in `.validate_rscales()`:
      x `rscales` must be a non-negative numeric vector with no NA values.
      i Got 1 NA value(s) and/or 0 negative value(s).
      v Supply a numeric vector of length 2 with all values >= 0.

# .validate_rscales() errors for negative values via as_survey_nonprob()

    Code
      as_survey_nonprob(df, weights = w, repweights = c(rw1, rw2), rscales = c(1,
        -0.5))
    Condition
      Error in `.validate_rscales()`:
      x `rscales` must be a non-negative numeric vector with no NA values.
      i Got 0 NA value(s) and/or 1 negative value(s).
      v Supply a numeric vector of length 2 with all values >= 0.

# .validate_rscales() errors for NA values via as_survey_replicate()

    Code
      as_survey_replicate(df_r, weights = wt, repweights = all_of(repwt_cols), type = "BRR",
      rscales = bad_rscales)
    Condition
      Error in `.validate_rscales()`:
      x `rscales` must be a non-negative numeric vector with no NA values.
      i Got 1 NA value(s) and/or 0 negative value(s).
      v Supply a numeric vector of length 25 with all values >= 0.

