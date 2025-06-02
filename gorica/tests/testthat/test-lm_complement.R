res <- lm(Sepal.Length ~ Species-1, iris)

est <- get_estimates(res)

test_that("get_estimates works", {
  expect_equivalent(est$estimate, c(5.006, 5.936, 6.588))
})

gor <- gorica(res, "Speciessetosa < (Speciesversicolor, Speciesvirginica)", comparison = "complement")

test_that("complement smaller than hyp", {
  expect_true(gor$fit$gorica_weights[2] < gor$fit$gorica_weights[1])
})

gor <- gorica(res, "Speciessetosa > (Speciesversicolor, Speciesvirginica)", comparison = "complement")

test_that("complement bigger than hyp", {
  expect_true(gor$fit$gorica_weights[1] < gor$fit$gorica_weights[2])
})
