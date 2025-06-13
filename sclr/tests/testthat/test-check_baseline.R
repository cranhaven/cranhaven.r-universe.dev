# Test baseline boundary
# Arseniy Khvorov
# Created 2019/11/04
# Last edit 2019/11/04

test_that("baseline boundary is checked", {
  expect_message(
    check_baseline(status ~ logHI, one_titre_data),
    "unlikely baseline of 1"
  )
  l1 <- sclr_ideal_data(n = 50, theta = 1e6)
  expect_message(
    check_baseline(status ~ logHI, l1),
    "likely baseline of 1"
  )
  fit_sclr <- sclr(status ~ logHI, one_titre_data)
  fit_lr <- glm(status ~ logHI, binomial(link = "logit"), one_titre_data)
  expect_message(check_baseline(fit_sclr = fit_sclr, fit_lr = fit_lr))
  expect_message(check_baseline(fit_sclr = fit_sclr))
  expect_message(check_baseline(fit_lr = fit_lr))
})

test_that("baseline boundary throws errors when used incorrectly", {
  fit_sclr <- sclr(status ~ logHI, one_titre_data)
  fit_lr <- glm(status ~ logHI, binomial(link = "logit"), two_titre_data)
  fit_sclr2 <- sclr(status ~ logHI, two_titre_data)
  fit_lr2 <- glm(
    status ~ logHI + logNI, binomial(link = "logit"), two_titre_data
  )
  expect_error(
    check_baseline(),
    "specify formula or at least one fit object"
  )
  expect_error(
    check_baseline(fit_lr = fit_lr, fit_sclr = fit_sclr),
    "the two fits must fit the same data"
  )
  expect_error(
    check_baseline(fit_lr = fit_lr, fit_sclr = fit_sclr),
    "the two fits must fit the same data"
  )
  expect_error(
    check_baseline(fit_lr = fit_lr2, fit_sclr = fit_sclr2),
    "the two fits must fit the same model"
  )
})
