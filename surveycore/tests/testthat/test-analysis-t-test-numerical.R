# tests/testthat/test-analysis-t-test-numerical.R
#
# Numerical oracle tests: get_t_test() vs survey::svyttest()
# All blocks guarded with skip_if_not_installed("survey")

skip_on_cran()

# ── Shared NHANES fixture ───────────────────────────────────────────────────

.make_nhanes_designs <- function() {
  skip_if_not_installed("survey")
  # Filter to exam sample (positive wtmec2yr) to satisfy weight validation
  nhanes_sub <- nhanes_2017[nhanes_2017$wtmec2yr > 0, ]
  nhanes_sub$gender <- factor(
    nhanes_sub$riagendr,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )
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
  list(sc = sc, sv = sv)
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. get_t_test() vs survey::svyttest() — NHANES
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_t_test() estimate matches svyttest() [numerical]", {
  skip_if_not_installed("survey")
  designs <- .make_nhanes_designs()
  t1 <- get_t_test(designs$sc, bpxsy1, by = gender, variance = c("se", "ci"))
  sv <- survey::svyttest(bpxsy1 ~ gender, designs$sv, na.action = na.omit)
  expect_equal(
    t1$estimate,
    sv$estimate[["difference in mean"]],
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("get_t_test() t_stat matches svyttest() [numerical]", {
  skip_if_not_installed("survey")
  designs <- .make_nhanes_designs()
  t1 <- get_t_test(designs$sc, bpxsy1, by = gender)
  sv <- survey::svyttest(bpxsy1 ~ gender, designs$sv, na.action = na.omit)
  expect_equal(t1$t_stat, sv$statistic[["t"]], tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("get_t_test() df matches svyttest() [numerical]", {
  skip_if_not_installed("survey")
  designs <- .make_nhanes_designs()
  t1 <- get_t_test(designs$sc, bpxsy1, by = gender)
  sv <- survey::svyttest(bpxsy1 ~ gender, designs$sv, na.action = na.omit)
  expect_equal(t1$df, sv$parameter[["df"]], tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("get_t_test() p_value matches svyttest() [numerical]", {
  skip_if_not_installed("survey")
  designs <- .make_nhanes_designs()
  t1 <- get_t_test(designs$sc, bpxsy1, by = gender)
  sv <- survey::svyttest(bpxsy1 ~ gender, designs$sv, na.action = na.omit)
  expect_equal(
    t1$p_value,
    sv$p.value[["genderFemale"]],
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("get_t_test() CI bounds match svyttest() [numerical]", {
  skip_if_not_installed("survey")
  designs <- .make_nhanes_designs()
  t1 <- get_t_test(designs$sc, bpxsy1, by = gender, variance = c("se", "ci"))
  sv <- survey::svyttest(bpxsy1 ~ gender, designs$sv, na.action = na.omit)
  expect_equal(t1$ci_low, sv$conf.int[1L, "2.5 %"],   tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(t1$ci_high, sv$conf.int[1L, "97.5 %"], tolerance = 1e-6, ignore_attr = TRUE)
})

# ═══════════════════════════════════════════════════════════════════════════
# 2. get_pairwise() consistency with get_t_test() on 2-level by
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_pairwise() on 2-level by matches get_t_test() [numerical]", {
  skip_if_not_installed("survey")
  designs <- .make_nhanes_designs()
  # Both should produce identical estimate, se, and p_value
  t1 <- get_t_test(designs$sc, bpxsy1, by = gender, variance = c("se", "ci"))
  pw <- get_pairwise(
    designs$sc,
    bpxsy1,
    by = gender,
    pval_adj = "none",
    variance = c("se", "ci")
  )
  expect_equal(pw$estimate, t1$estimate, tolerance = 1e-10)
  expect_equal(pw$se,       t1$se,       tolerance = 1e-8)
  expect_equal(
    pw$p_value,
    t1$p_value,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(pw$ci_low,   t1$ci_low,   tolerance = 1e-6)
  expect_equal(pw$ci_high,  t1$ci_high,  tolerance = 1e-6)
})
