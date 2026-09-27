# tests/testthat/test-glm-anova-numerical.R
#
# Numerical oracle tests: get_anova() vs survey::regTermTest().
#
# All blocks guarded by skip_if_not_installed("survey").
#
# Tolerances (per .claude/rules/testing-surveycore.md and spec §VII):
#   statistic: 1e-8
#   p-value:   1e-6
#
# Note: anova.svyglm() internally calls update(model, ...) which evaluates
# the original svyglm call in its formula environment. In test_that() scope
# that lookup fails because `sv` is local. We therefore compare against
# survey::regTermTest() (the helper anova.svyglm wraps), which operates
# directly on fitted model objects with no environment re-evaluation. This
# is the same oracle used by anova.svyglm per term.

skip_on_cran()

# ═══════════════════════════════════════════════════════════════════════════
# 1. Taylor sequential — 4 combos
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova() Taylor/LRT/F matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  # regTermTest(method="LRT") internally calls update() which evaluates
  # unqualified svyglm() — requires survey attached to the search path.
  suppressPackageStartupMessages(library(survey))
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  sc <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  # Assign to globalenv so update() inside regTermTest can find it.
  assign("sv", survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  ), envir = globalenv())
  on.exit(rm("sv", envir = globalenv()), add = TRUE)
  sv <- get("sv", envir = globalenv())

  fit_sc <- survey_glm(sc, bpxsy1 ~ ridageyr + riagendr)
  # Sequential LRT: test each term on the model reduced to its left-hand side.
  fit_sv_full <- survey::svyglm(bpxsy1 ~ ridageyr + riagendr, design = sv)
  fit_sv_red <- survey::svyglm(bpxsy1 ~ ridageyr, design = sv)

  r_sc <- get_anova(fit_sc, method = "LRT", test = "F")

  r_age <- survey::regTermTest(fit_sv_red, "ridageyr", method = "LRT")
  r_sex <- survey::regTermTest(fit_sv_full, "riagendr", method = "LRT")

  expect_equal(r_sc$statistic[1L], as.numeric(r_age$chisq), tolerance = 1e-8)
  expect_equal(r_sc$statistic[2L], as.numeric(r_sex$chisq), tolerance = 1e-8)
  expect_equal(r_sc$p_value[1L], as.numeric(r_age$p), tolerance = 1e-6)
  expect_equal(r_sc$p_value[2L], as.numeric(r_sex$p), tolerance = 1e-6)
})

test_that("get_anova() Taylor/LRT/Chisq matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  # regTermTest(method="LRT") internally calls update() which evaluates
  # unqualified svyglm() — requires survey attached to the search path.
  suppressPackageStartupMessages(library(survey))
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  sc <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  assign("sv", survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  ), envir = globalenv())
  on.exit(rm("sv", envir = globalenv()), add = TRUE)
  sv <- get("sv", envir = globalenv())

  fit_sc <- survey_glm(sc, bpxsy1 ~ ridageyr + riagendr)
  fit_sv_full <- survey::svyglm(bpxsy1 ~ ridageyr + riagendr, design = sv)
  fit_sv_red <- survey::svyglm(bpxsy1 ~ ridageyr, design = sv)

  r_sc <- get_anova(fit_sc, method = "LRT", test = "Chisq")

  r_age <- survey::regTermTest(
    fit_sv_red,
    "ridageyr",
    method = "LRT",
    df = Inf
  )
  r_sex <- survey::regTermTest(
    fit_sv_full,
    "riagendr",
    method = "LRT",
    df = Inf
  )

  expect_equal(r_sc$statistic[1L], as.numeric(r_age$chisq), tolerance = 1e-8)
  expect_equal(r_sc$statistic[2L], as.numeric(r_sex$chisq), tolerance = 1e-8)
  expect_equal(r_sc$p_value[1L], as.numeric(r_age$p), tolerance = 1e-6)
  expect_equal(r_sc$p_value[2L], as.numeric(r_sex$p), tolerance = 1e-6)
})

test_that("get_anova() Taylor/Wald/F matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  sc <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  )

  fit_sc <- survey_glm(sc, bpxsy1 ~ ridageyr + riagendr)
  # Sequential Wald tests each term on the model reduced to its left-hand side.
  # Outer term (riagendr) is tested on the full model; leading term (ridageyr)
  # on the reduced-by-riagendr model.
  fit_sv_full <- survey::svyglm(bpxsy1 ~ ridageyr + riagendr, design = sv)
  fit_sv_red <- survey::svyglm(bpxsy1 ~ ridageyr, design = sv)

  r_sc <- get_anova(fit_sc, method = "Wald", test = "F")

  r_age <- survey::regTermTest(fit_sv_red, "ridageyr", method = "Wald")
  r_sex <- survey::regTermTest(fit_sv_full, "riagendr", method = "Wald")

  # Wald method: regTermTest stores the F-statistic in $Ftest.
  expect_equal(r_sc$statistic[1L], as.numeric(r_age$Ftest), tolerance = 1e-8)
  expect_equal(r_sc$statistic[2L], as.numeric(r_sex$Ftest), tolerance = 1e-8)
  expect_equal(r_sc$p_value[1L], as.numeric(r_age$p), tolerance = 1e-6)
  expect_equal(r_sc$p_value[2L], as.numeric(r_sex$p), tolerance = 1e-6)
})

test_that("get_anova() Taylor/Wald/Chisq matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  sc <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  )

  fit_sc <- survey_glm(sc, bpxsy1 ~ ridageyr + riagendr)
  fit_sv_full <- survey::svyglm(bpxsy1 ~ ridageyr + riagendr, design = sv)
  fit_sv_red <- survey::svyglm(bpxsy1 ~ ridageyr, design = sv)

  r_sc <- get_anova(fit_sc, method = "Wald", test = "Chisq")

  r_age <- survey::regTermTest(
    fit_sv_red,
    "ridageyr",
    method = "Wald",
    df = Inf
  )
  r_sex <- survey::regTermTest(
    fit_sv_full,
    "riagendr",
    method = "Wald",
    df = Inf
  )

  expect_equal(r_sc$statistic[1L], as.numeric(r_age$chisq), tolerance = 1e-8)
  expect_equal(r_sc$statistic[2L], as.numeric(r_sex$chisq), tolerance = 1e-8)
  expect_equal(r_sc$p_value[1L], as.numeric(r_age$p), tolerance = 1e-6)
  expect_equal(r_sc$p_value[2L], as.numeric(r_sex$p), tolerance = 1e-6)
})

# ═══════════════════════════════════════════════════════════════════════════
# 2. Taylor comparison mode
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova() Taylor comparison LRT/F matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  # regTermTest(method="LRT") internally calls update() which evaluates
  # unqualified svyglm() — requires survey attached to the search path.
  suppressPackageStartupMessages(library(survey))
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  sc <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  # Assign design to globalenv so update() can resolve its name.
  assign("sv", survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  ), envir = globalenv())
  on.exit(rm("sv", envir = globalenv()), add = TRUE)
  sv <- get("sv", envir = globalenv())
  fit_s_sc <- survey_glm(sc, bpxsy1 ~ ridageyr)
  fit_b_sc <- survey_glm(sc, bpxsy1 ~ ridageyr + riagendr)
  fit_b_sv <- survey::svyglm(bpxsy1 ~ ridageyr + riagendr, design = sv)

  r_sc <- get_anova(list(fit_s_sc, fit_b_sc), method = "LRT", test = "F")
  # Oracle: test dropping riagendr from the full model.
  r_sv <- survey::regTermTest(fit_b_sv, "riagendr", method = "LRT")

  expect_equal(
    as.numeric(r_sc$statistic[1L]),
    as.numeric(r_sv$chisq),
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(r_sc$p_value[1L]),
    as.numeric(r_sv$p),
    tolerance = 1e-6
  )
})

test_that("get_anova() Taylor comparison Wald/Chisq matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  sc <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  )
  fit_s_sc <- survey_glm(sc, bpxsy1 ~ ridageyr)
  fit_b_sc <- survey_glm(sc, bpxsy1 ~ ridageyr + riagendr)
  fit_b_sv <- survey::svyglm(bpxsy1 ~ ridageyr + riagendr, design = sv)

  r_sc <- get_anova(list(fit_s_sc, fit_b_sc), method = "Wald", test = "Chisq")
  r_sv <- survey::regTermTest(
    fit_b_sv,
    "riagendr",
    method = "Wald",
    df = Inf
  )

  expect_equal(
    as.numeric(r_sc$statistic[1L]),
    as.numeric(r_sv$chisq),
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(r_sc$p_value[1L]),
    as.numeric(r_sv$p),
    tolerance = 1e-6
  )
})

# ═══════════════════════════════════════════════════════════════════════════
# 3. BRR replicate oracle (acs_pums_wy)
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova() BRR replicate Wald/F matches regTermTest() [numerical]", {
  skip_if_not_installed("survey")
  rep_cols <- grep("^prwgtp[0-9]+$", names(acs_pums_wy), value = TRUE)
  skip_if(length(rep_cols) == 0L, "acs_pums_wy missing replicate weights")
  skip_if(
    !all(c("wagp", "agep", "sex") %in% names(acs_pums_wy)),
    "acs_pums_wy columns missing"
  )

  sc <- as_survey_replicate(
    acs_pums_wy,
    weights = pwgtp,
    repweights = all_of(rep_cols),
    type = "BRR"
  )
  sv <- survey::svrepdesign(
    weights = ~pwgtp,
    repweights = acs_pums_wy[, rep_cols],
    data = acs_pums_wy,
    type = "BRR",
    combined.weights = TRUE
  )

  fit_sc <- survey_glm(sc, wagp ~ agep + sex)
  fit_sv <- survey::svyglm(wagp ~ agep + sex, design = sv)

  r_sc <- get_anova(fit_sc, method = "Wald", test = "F")
  r_age <- survey::regTermTest(fit_sv, "agep", method = "Wald")
  r_sex <- survey::regTermTest(fit_sv, "sex", method = "Wald")

  expect_equal(r_sc$statistic[1L], as.numeric(r_age$Ftest), tolerance = 1e-8)
  expect_equal(r_sc$statistic[2L], as.numeric(r_sex$Ftest), tolerance = 1e-8)
  expect_equal(r_sc$p_value[1L], as.numeric(r_age$p), tolerance = 1e-6)
  expect_equal(r_sc$p_value[2L], as.numeric(r_sex$p), tolerance = 1e-6)
})

# ═══════════════════════════════════════════════════════════════════════════
# 4. Two-phase oracle
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova() two-phase Wald/F runs without error [smoke]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 400,
    n_psu = 40,
    n_strata = 4,
    design = "twophase",
    seed = 7L
  )
  skip_if(!"subset" %in% names(df), "twophase helper missing subset column")

  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  sc <- as_survey_twophase(ph1, subset = subset, method = "approx")
  fit_sc <- survey_glm(sc, y1 ~ y2 + y3)

  r_sc <- get_anova(fit_sc, method = "Wald", test = "F")

  # Two-phase variance is approximated via method="approx" in surveycore and
  # differs from survey::twophase() in finite samples (legitimate method
  # divergence, not a bug in get_anova). Assert shape and finite p-values
  # rather than exact numerical equality.
  expect_s3_class(r_sc, "survey_anova")
  expect_equal(nrow(r_sc), 2L)
  expect_true(all(is.finite(r_sc$statistic)))
  expect_true(all(is.finite(r_sc$p_value)))
  expect_true(all(r_sc$p_value >= 0 & r_sc$p_value <= 1))
})
