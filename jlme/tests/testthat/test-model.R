skip_conditionally()

stop_julia()
print(
  jlme_status()
)

print(Sys.which("julia"))

print(system.time({
  jlme_setup(restart = TRUE, verbose = TRUE)
}))

print(
  jlme_status()
)

print(
  jlme_setup(restart = FALSE)
)

test_that("reproduces `lm()` and `lmer()` outputs", {

  fm1 <- mpg ~ hp
  jmod1 <- jlm(fm1, mtcars)
  expect_s3_class(jmod1, "jlme")
  expect_s3_class(tidy(jmod1), "data.frame")
  expect_s3_class(glance(jmod1), "data.frame")
  rmod1 <- lm(fm1, mtcars)
  expect_similar_models(jmod1, rmod1)

  fm2 <- Reaction ~ Days + (Days | Subject)
  jmod2 <- jlmer(fm2, lme4::sleepstudy, REML = TRUE)
  expect_s3_class(jmod2, "jlme")
  expect_s3_class(tidy(jmod2), "data.frame")
  expect_s3_class(glance(jmod2), "data.frame")
  rmod2 <- lme4::lmer(fm2, lme4::sleepstudy)
  expect_similar_models(jmod2, rmod2)

  fm3 <- r2 ~ Anger + (1|id)
  jmod3 <- jlmer(fm3, lme4::VerbAgg, family = "binomial")
  rmod3 <- lme4::glmer(fm3, lme4::VerbAgg, family = "binomial")
  expect_similar_models(jmod3, rmod3)

  fm4 <- am ~ mpg
  jmod4 <- jlm(fm4, mtcars, family = "binomial")
  rmod4 <- glm(fm4, mtcars, family = "binomial")
  expect_similar_models(jmod4, rmod4)

  # print/format works
  expect_output({
    # Fixed
    print(jmod1)
    # Mixed
    print(jmod2)
    print(jmod2, format = "markdown")
  })

  # optsum set
  unfit <- jlmer(
    Reaction ~ Days + (1 | Subject),
    lme4::sleepstudy,
    fit = FALSE,
    optsum = list(maxfeval = 10L)
  )
  expect_identical(unfit$optsum$maxfeval, 10L)

})

test_that("preserves contrasts", {

  df <- mtcars[, c("mpg", "cyl", "am")]

  rj_fit <- function(df) {
    fm <- mpg ~ cyl * am
    list(r = lm(fm, df), j = jlm(fm, df))
  }

  # Numeric covariates
  expect_similar_models(rj_fit(df))

  # Default contrasts
  df$cyl <- as.factor(df$cyl)
  expect_similar_models(rj_fit(df))
  df$am <- as.factor(df$am)
  expect_similar_models(rj_fit(df))

  # Custom contrasts (sum)
  contrasts(df$am) <- contr.sum(2)
  expect_similar_models(rj_fit(df))

  # Custom contrasts (helmert; named)
  contrasts(df$cyl) <- contr.helmert(3)
  colnames(contrasts(df$cyl)) <- c("4vs6", "4&6vs8")
  expect_similar_models(rj_fit(df))

})

test_that("formula conversions work", {

  if (requireNamespace("JuliaFormulae", quietly = TRUE)) {
    zcp <- Reaction ~ Days + (1 + Days || Subject)
    j_zcp <- jlmer(zcp, lme4::sleepstudy, REML = TRUE)
    r_zcp <- lme4::lmer(zcp, lme4::sleepstudy)
    expect_similar_models(j_zcp, r_zcp)

    protect <- Reaction ~ I(Days + 100) + (1 | Subject)
    j_protect <- jlmer(protect, lme4::sleepstudy, REML = TRUE)
    r_protect <- lme4::lmer(protect, lme4::sleepstudy)
    expect_similar_models(j_protect, r_protect, ignore_names = TRUE)

    interactions <- mpg ~ am + cyl + am:cyl
    j_interactions <- jlm(interactions, mtcars)
    r_interactions <- lm(interactions, mtcars)
    expect_similar_models(j_interactions, r_interactions)
  } else {
    message("Skipping formula conversion tests - JuliaFormulae not installed.")
  }

})

test_that("all-julia inputs work", {

  fm <- mpg ~ hp
  df <- mtcars
  mod1 <- jlm(fm, df)

  fm_jl <- jl_formula(fm)
  df_jl <- jl_data(df)
  mod2 <- jlm(fm_jl, df_jl)

  expect_similar_models(mod1, mod2)

})

test_that("misc jl safety and flexibility behaviors", {

  # Captures julia parsing error of invalid formula
  ## Invalid parse caught at JuliaFormulae
  expect_error(jl_formula("a ~ b.c"))
  expect_error(jl_formula("a ~ b&2"))

  # Pretty-print contrasts
  df <- mtcars
  df$am <- as.factor(df$am)
  contrasts(df$am) <- contr.sum(2)
  expect_output(
    # Expect output to console
    jl_contrasts(df, show_code = TRUE)
  )

  # Family argument spec
  expect_error({
    jl_family(mean)
  })
  expect_no_error({
    family_specs <- list(
      jl_family("binomial"),
      jl_family(binomial),
      jl_family(binomial())
    )
  })
  expect_identical(
    sapply(family_specs, jl_get),
    rep(jl_get(jl_family("binomial")), 3)
  )

})

test_that("re-exports work", {
  x <- jlmer(r2 ~ Anger + (1 | id), lme4::VerbAgg, family = "binomial")
  expect_type(propertynames(x), "character")
  expect_type(issingular(x), "logical")
  x_reduced <- jlmer(r2 ~ 1 + (1 | id), lme4::VerbAgg, family = "binomial")
  expect_true(is_jl(likelihoodratiotest(x, x_reduced)))
})

test_that("CI functions work", {

  jmod <- jlmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  tidied <- tidy(jmod)
  expect_no_error({
    tidy(jmod, effects = "var_model")
    tidy(jmod, effects = "fixed")
    tidy(jmod, effects = "ran_pars")
  })

  prof <- profilelikelihood(jmod)
  tidied_prof <- tidy(prof)
  expect_true(is_jl(prof))
  expect_s3_class(tidied_prof, "data.frame")
  expect_contains(tidied$term, tidied_prof$term)
  expect_no_error({
    tidy(prof, effects = "var_model")
    tidy(prof, effects = "fixed")
    tidy(prof, effects = "ran_pars")
  })

  samp <- parametricbootstrap(jmod, 10L, 1L)
  tidied_samp <- tidy(samp)
  expect_true(is_jl(samp))
  expect_s3_class(tidied_samp, "data.frame")
  expect_contains(tidied$term, tidied_samp$term)
  expect_no_error({
    tidy(samp, effects = "var_model")
    tidy(samp, effects = "fixed")
    tidy(samp, effects = "ran_pars")
  })

})

stop_julia()
