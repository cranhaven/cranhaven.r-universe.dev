test_that("test surr_rsq", {
  # library(SurrogateRsq)
  library(MASS)
  library(dplyr)
  # Read the csv file of the white wine data
  data("WhiteWine")

  WhiteWine <- WhiteWine %>% mutate(quality = as.ordered(quality))
  y <- "quality"

  # Find all covariates
  x_names <- colnames(WhiteWine)[!(colnames(WhiteWine) %in% y)]

  full_formula <- as.formula(paste(y, paste(x_names, collapse = " + "), sep = " ~ "))

  full_mod <- polr(formula = full_formula,
                   data = WhiteWine,
                   method = "probit")

  allsig_formula <- update(full_formula, ~ . - citric.acid - chlorides - total.sulfur.dioxide)

  allsig_mod <- polr(formula = allsig_formula,
                     data = WhiteWine,
                     method = "probit")

  sur2 <- surr_rsq(model = allsig_mod,
                   full_model = full_mod,
                   # data = WhiteWine,
                   avg.num = 100)
  expect_type(sur2, "list")
  expect_lt(object = sur2$surr_rsq, expected = 0.33)
})


test_that("test surr_rsq which==Surrogate for plor", {
  library(dplyr)
  data("RedWine")

  full_formula <- as.formula(quality ~ fixed.acidity + volatile.acidity +
                               citric.acid+ residual.sugar + chlorides + free.sulfur.dioxide +
                               total.sulfur.dioxide + density + pH + sulphates + alcohol)

  full_mod <- polr(formula = full_formula,
                   data=RedWine, method  = "probit")

  select_model <- update(full_mod, formula. = ". ~ . - fixed.acidity -
                       citric.acid - residual.sugar - density")

  # all.equal(select_model$model, (full_mod$model[,names(select_model$model)]),
  #           check.attributes = FALSE)

  surr_obj_sele_mod <- surr_rsq(model = select_model,
                                full_model = full_mod,
                                avg.num = 30)

  expect_type(surr_obj_sele_mod, "list")
})
