# get_anova() rejects non-survey_glm_fit input (A-23)

    Code
      get_anova("not a fit")
    Condition
      Error in `get_anova()`:
      x `object` must be a <survey_glm_fit>, a list of <survey_glm_fit> objects, or a survey design.
      i Got <character>.

# get_anova() rejects null with method='LRT' (A-4)

    Code
      get_anova(fit, method = "LRT", null = 0)
    Condition
      Error in `get_anova()`:
      x `null` is only valid with `method = "Wald"`.
      v Set `method = "Wald"` to test a non-zero null hypothesis.

# get_anova() errors with insufficient residual df (A-9)

    Code
      get_anova(fit)
    Condition
      Error in `get_anova()`:
      x Design has -3 residual degrees of freedom for a model with 6 coefficients; at least 1 is required.
      i The reference distribution cannot be calibrated at this ddf and the p-value is not well-defined for either "F" or "Chisq" tests.
      v Fit a simpler model (fewer coefficients) or use a design with more primary sampling units.

# get_anova() errors when null length mismatches q (A-10)

    Code
      get_anova(fit, method = "Wald", null = c(0, 0))
    Condition
      Error in `.reg_term_test()`:
      x `null` has length 2 but 1 coefficient are tested for term y3.
      v Supply one value per tested coefficient, or `null = NULL`.

# get_anova() LRT errors when fit_ is NULL (A-11 sequential)

    Code
      get_anova(fit_stripped, method = "LRT")
    Condition
      Error in `get_anova()`:
      x Variance / n-invariance checks require the underlying GLM fit object(s), which are not persisted after serialization.
      i Sequential LRT needs model@fit_.
      v Re-fit the model in the current R session. Sequential Wald is unaffected and remains serialization-safe.

# get_anova() errors on intercept-only model (A-17)

    Code
      get_anova(fit)
    Condition
      Error in `.anova_sequential()`:
      x Model is intercept-only; there are no terms to test.
      i Sequential ANOVA requires at least one non-intercept term in the formula.
      v Fit a model with at least one predictor, e.g. `y ~ x`.

# get_anova() rejects non-numeric tolerance (A-18)

    Code
      get_anova(fit, tolerance = "1e-5")
    Condition
      Error in `get_anova()`:
      x `tolerance` must be a single finite non-negative numeric value.
      i Got <character> of length 1: "1e-5".
      v Use the default `sqrt(.Machine$double.eps)`, or supply any "0"-or-positive numeric scalar.

# get_anova() print snapshot: Taylor + LRT/F + decimals=3

    Code
      print(get_anova(fit, method = "LRT", test = "F", decimals = 3))
    Output
      # A survey_anova result (Rao-Scott LRT, F reference)
      # Model: y1 ~ y2 + y3
      # Design: Taylor series | N: 300 | Design df: 24
      # A tibble: 2 x 7
        term  statistic    df   ddf  deff p_value stars
        <chr>     <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>
      1 y2         255.     1    24  82.8   0.094 "."  
      2 y3         105.     1    23  94.4   0.306 ""   

# get_anova() print snapshot: Taylor + Wald suppresses deff column in body

    Code
      print(r)
    Output
      # A survey_anova result (Design-based Wald, F reference)
      # Model: y1 ~ y2 + y3
      # Design: Taylor series | N: 300 | Design df: 24
      # A tibble: 2 x 6
        term  statistic    df   ddf p_value stars
        <chr>     <dbl> <dbl> <dbl>   <dbl> <chr>
      1 y2         3.08     1    24   0.092 "."  
      2 y3         1.11     1    23   0.304 ""   

# get_anova() print snapshot: BRR replicate + LRT/F

    Code
      print(get_anova(fit, method = "LRT", test = "F", decimals = 3))
    Output
      # A survey_anova result (Rao-Scott LRT, F reference)
      # Model: y1 ~ y2 + y3
      # Design: Replicate weights (BRR) | N: 100 | Design df: 3
      # A tibble: 2 x 7
        term  statistic    df   ddf  deff p_value stars
        <chr>     <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>
      1 y2         26.5     1     3 0.555   0.007 **   
      2 y3        237.      1     2 0.765   0.004 **   

# get_anova() errors when models are not nested (A-2)

    Code
      get_anova(list(fit_a, fit_b))
    Condition
      Error in `.anova_compare()`:
      x Models are not symbolically nested.
      i Neither model's term set is a subset of the other.
      v Refit one model so its terms are a superset of the other's.

# get_anova() errors when responses differ (A-3)

    Code
      get_anova(list(fit_y1, fit_y3))
    Condition
      Error in `.anova_compare()`:
      x Models have different response variables.
      i y1 vs. y3.

# get_anova() errors when @data differs (A-5, case a)

    Code
      get_anova(list(fit_1, fit_2))
    Condition
      Error in `.anova_compare()`:
      x Models were fit on designs with different @data or @variables slots.
      i Comparison mode requires both fits to reference semantically-identical designs (same rows, same design variables).
      v Confirm the two designs wrap the same data frame and use the same `ids` / `weights` / `strata` / `fpc` columns; typically this means fitting both models against one stored design object.

# anova(fit, fit2, fit3) errors with A-7 (too many extras)

    Code
      anova(fit_1, fit_2, fit_3)
    Condition
      Error in `anova()`:
      x anova() on <survey_glm_fit> accepts at most one additional <survey_glm_fit> model.
      i Got 2 extra argument(s).

# anova(fit, 'not a fit') errors with A-7 (bad dots type)

    Code
      anova(fit, "not a fit")
    Condition
      Error in `anova()`:
      x anova() on <survey_glm_fit> accepts at most one additional <survey_glm_fit> model.
      i Got 1 extra argument(s).

# get_anova(list(fit, fit)) errors with identical term sets (A-16)

    Code
      get_anova(list(fit, fit))
    Condition
      Error in `.anova_compare()`:
      x Both models have identical term sets; there are no terms to test.
      i Comparison mode requires one model's terms to be a strict superset of the other's.
      v Use `get_anova(model)` for sequential term tests, or fit one of the models with additional terms.

