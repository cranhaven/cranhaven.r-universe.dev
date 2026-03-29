#------------------------------------
# test carbon interconversion functions

load("mlo.Rdata")

ref_data <- data.frame(d2H_ref_mean  = c(-80, -100, -120),
                       d18O_ref_mean = c(-10, -11, -12))

test_that("reference water with reasonable d-excess is unchanged", {
  expect_equal(swap_standard_isotoperatios(ref_data), ref_data)
})

ref_data2 <- data.frame(d2H_ref_mean = c(-10, -11, -12),
                        d18O_ref_mean = c(-80, -100, -120))

test_that("unreasonable d-excess values get swapped", {
  expect_equal(swap_standard_isotoperatios(ref_data2), ref_data)
})

test_vals <- c(0, -5, -10, -20, -30)
test_rc    <- delta_to_R(test_vals[3], "carbon")
test_ro    <- delta_to_R(test_vals[3], "oxygen")
test_rh    <- delta_to_R(test_vals[3], "hydrogen")

test_that("R_to_delta and delta_to_R are invertible", {
  expect_equal(R_to_delta(delta_to_R(mlo$d13c, "carbon"),
                          "carbon"),
               mlo$d13c)
  expect_equal(R_to_delta(delta_to_R(test_vals, "oxygen"),
                          "oxygen"),
               test_vals)
  expect_equal(R_to_delta(delta_to_R(test_vals, "hydrogen"),
                          "hydrogen"),
               test_vals)
})

test_that("R values are < 1", {
  expect_lt(delta_to_R(test_vals[3], "carbon"), 1)
  expect_lt(delta_to_R(test_vals[3], "oxygen"), 1)
  expect_lt(delta_to_R(test_vals[3], "hydrogen"), 1)
})

test_that("delta values are < 50", {
  expect_lt(R_to_delta(test_rc, "carbon"), 50)
  expect_lt(R_to_delta(test_ro, "oxygen"), 50)
  expect_lt(R_to_delta(test_rh, "hydrogen"), 50)
})

test_that("delta values are > -1000", {
  expect_gte(R_to_delta(test_rc, "carbon"), -1000)
  expect_gte(R_to_delta(test_ro, "oxygen"), -1000)
  expect_gte(R_to_delta(test_rh, "hydrogen"), -1000)
})

test_that("12co2 calculations work", {
  expect_equal(calculate_12CO2(mlo$co2, mlo$d13c),
               mlo$c12_co2, tolerance = 0.01)
})

test_that("13co2 calculations work", {
  expect_equal(calculate_13CO2(mlo$co2, mlo$d13c),
               mlo$c13_co2, tolerance = 0.01)
})
