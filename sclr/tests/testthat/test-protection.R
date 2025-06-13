# Test protection calculations 
# Arseniy Khvorov
# Created 2019/10/23
# Last edit 2019/10/23

library(sclr)

fit <- sclr(status ~ logHI, one_titre_data)

test_that("protective titre is found correctly", {
  prot50_point <- find_prot_titre_val(fit, "logHI")
  expect_equal(predict(fit, prot50_point)$prot_point, 0.5, tol = 1e-6)
  prot50_low <- find_prot_titre_val(fit, "logHI", prot_var_name = "prot_l")
  expect_equal(predict(fit, prot50_low)$prot_l, 0.5, tol = 1e-6)
  prot50_high <- find_prot_titre_val(fit, "logHI", prot_var_name = "prot_u")
  expect_equal(predict(fit, prot50_high)$prot_u, 0.5, tol = 1e-6)
})

test_that("protection level calculation works", {
  prot50full <- get_protection_level(fit, "logHI")
  expect_named(prot50full, c("logHI", "prot_prob", "est"))
})
