# get_anova(<character>) snapshot matches (A-21)

    Code
      get_anova("not a fit")
    Condition
      Error in `get_anova()`:
      x `object` must be a <survey_glm_fit>, a list of <survey_glm_fit> objects, or a survey design.
      i Got <character>.

# get_anova(list mixed classes) snapshot matches (A-21)

    Code
      get_anova(list(fit, "not a fit", fit))
    Condition
      Error in `.anova_dispatch_list()`:
      x All elements of `object` must inherit <survey_glm_fit>.
      i Bad element(s) at position(s) 2: <character>.

# get_anova(list()) snapshot matches (A-22)

    Code
      get_anova(list())
    Condition
      Error in `.anova_dispatch_list()`:
      x `object` is a zero-length list.
      v Supply at least one <survey_glm_fit>.

# get_anova(design) without formula snapshot matches (A-23)

    Code
      get_anova(d)
    Condition
      Error in `.anova_dispatch_design()`:
      x `formula` (or `response` / `predictors`) is required when `object` is a survey design.
      v Supply a model formula, e.g. `get_anova(design, y ~ x1 + x2)`.

# get_anova(fit, formula = ...) snapshot matches (A-24)

    Code
      get_anova(fit, formula = y1 ~ y2)
    Condition
      Error in `get_anova()`:
      x `formula` / `response` / `predictors` must be `NULL` when `object` is a <survey_glm_fit> or a list of fits.
      i These arguments only apply when `object` is a survey design.
      v Drop the formula argument, or pass a design as `object` instead.

# get_anova(list(fit1, fit2), null = 0) warning snapshot matches (A-25)

    Code
      r <- get_anova(list(fit_s, fit_b), null = 0, method = "Wald")
    Condition
      Warning:
      ! `null` has no effect when `object` is a list of fits.
      i `null` only applies to single-model anova.
      i Dropping `null` and continuing with model comparison.

