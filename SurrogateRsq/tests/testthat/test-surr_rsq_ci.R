test_that("test surr_rsq_ci", {
  # library(SurrogateRsq)
  # data("RedWine")

  # full_formula <- as.formula("quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol")

  # fullmodel <- polr(formula = full_formula,data=RedWine, method  = "probit")

  # select_model <- update(fullmodel, formula. = ". ~ . - fixed.acidity -
  # citric.acid - residual.sugar - density")

  # surr_rsq_select <- surr_rsq(select_model, fullmodel, avg.num = 30)

  # BS_fullmodel <- update(fullmodel, data = RedWine[sample(1:dim(RedWine)[1], dim(RedWine)[1], replace = T), ])
  # summary(fullmodel)
  # summary(BS_fullmodel)

  # CI_select <- surr_rsq_ci(surr_rsq_select, alpha = 0.05, B = 1000) # Not run, it takes time.
  # CI_select$surr_rsq
  # CI_select$surr_rsq_ci
  # hist(CI_select$surr_rsq_BS)
  # CI_select$data

  # expect_true(as.numeric(CI_select$surr_rsq_ci[2,1]) <= CI_select$surr_rsq, TRUE)
  # expect_true(as.numeric(CI_select$surr_rsq_ci[2,2]) >= CI_select$surr_rsq, TRUE)

})
