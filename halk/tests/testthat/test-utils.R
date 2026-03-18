test_that("check age and size data return errors when they're supposed to", {
  expect_error(check_age_data(laa_data, age_col = "foo"))
  expect_error(check_length_data(laa_data, age_col = "bar"))
})

test_that("check if spp_levels functions works correctly", {
  x <- c("county", "waterbody")
  y <- c("spp", x)
  z <- c("species", y)
  expect_true(is_spp_in_levels(y))
  expect_false(is_spp_in_levels(x))
  expect_true(is_spp_in_data(spp_data))
  expect_false(is_spp_in_data(laa_data))
  expect_equal(spp_level(y), c("spp"))
  expect_equal(spp_level(z), c("species", "spp"))
  expect_null(spp_level(x))
  expect_equal(rm_spp_level(y), x)
  expect_equal(rm_spp_level(z), x)
  expect_null(rm_spp_level("spp"))
  level_test <- suppressWarnings(add_spp_level(spp_data, levels = x))
  expect_equal(level_test, y)
  spp_level_test <-
    suppressWarnings(make_halk(spp_data, min_age_sample_size = 2))
  expect_equal(levels(spp_level_test), "spp")
  expect_warning(make_halk(spp_data, min_age_sample_size = 1))
})

test_that("ages_as_* functions return correct types", {
  ord_laa_data <- ages_as_ordered_factor(laa_data)
  int_laa_data <- ages_as_integer(ord_laa_data, age_col = "age")
  expect_true(is.ordered(ord_laa_data$age))
  expect_true(is.integer(int_laa_data$age))
  expect_equal(int_laa_data, laa_data)
})

test_that("ages are convereted to ordered and back to integer correctly", {
  ages <- 1:10
  ord_vec <- as.ordered(1:10)
  ord_ages <- ages_as_ordered_factor(ages)
  ord_age_df <- ages_as_ordered_factor(laa_data)
  age_integer_df <- ages_as_integer(ord_age_df, age_col = "age")
  expect_equal(ord_ages, ord_vec)
  expect_equal(ages_as_integer(ord_ages), ages)
  expect_equal(ages_as_integer(ord_vec), ages)
  expect(is.ordered(ord_age_df$age), failure_message = "Ages didn't convert")
  expect_equal(age_integer_df, laa_data)
})

test_that("check_model_types returns correct names", {
  expect_equal(check_model_type("alk"), "alk")
  expect_equal(check_model_type("age_length_key"), "alk")
  expect_equal(check_model_type("age_len_key"), "alk")
  expect_equal(check_model_type("age length key"), "alk")
  expect_equal(check_model_type("age-length key"), "alk")
  expect_equal(check_model_type("age-length-key"), "alk")
  expect_equal(check_model_type("halk"), "halk")
  expect_equal(check_model_type("smart_alk"), "halk")
  expect_equal(check_model_type("smart_age_length_key"), "halk")
  expect_equal(check_model_type("smart_age_len_key"), "halk")
  expect_equal(check_model_type("smart_age length key"), "halk")
  expect_equal(check_model_type("smart_age-length key"), "halk")
  expect_equal(check_model_type("smart_age-length-key"), "halk")
  expect_equal(check_model_type("rf"), "rf")
  expect_equal(check_model_type("random forest"), "rf")
  expect_equal(check_model_type("randomForest"), "rf")
  expect_equal(check_model_type("random_forest"), "rf")
  expect_equal(check_model_type("grad_boost"), "grad_boost")
  expect_equal(check_model_type("gbm"), "grad_boost")
  expect_equal(check_model_type("gradient boosting machine"), "grad_boost")
  expect_equal(check_model_type("gradient-boosting machine"), "grad_boost")
  expect_equal(check_model_type("gradient_boosting_machine"), "grad_boost")
  # expect_equal(check_model_type("ann"), "ann")
  # expect_equal(check_model_type("neural_network"), "ann")
  # expect_equal(check_model_type("neural network"), "ann")
  # expect_equal(check_model_type("artificial neural network"), "ann")
  # expect_equal(check_model_type("mm"), "mm")
  # expect_equal(check_model_type("mixture model"), "mm")
  # expect_equal(check_model_type("mixture_model"), "mm")
  expect_error(check_model_type("foobar"))
})

test_that("sanitize_laa_data works as intended", {
  test_dat <- data.frame(length = 1:3, age = 1:3)
  na_test_dat <- data.frame(length = c(1:3, NA), age = c(NA, 1:3))
  exp_na_test_dat <- data.frame(length = 2:3, age = 1:2)
  expect_equal(sanitize_laa_data(test_dat), test_dat)
  expect_equal(sanitize_laa_data(na_test_dat), exp_na_test_dat)
})

test_that("adjust_plus_min_ages_* functions correctly adjust age data", {
  expect_equal(min(adjust_plus_min_ages(laa_data, minage = 3)$age), 3)
  expect_equal(max(adjust_plus_min_ages(laa_data, pls_grp = 5)$age), 5)
  expect_equal(min(adjust_plus_min_ages(0:10, minage = 2)), 2)
  expect_equal(max(adjust_plus_min_ages(0:10, pls_grp = 5)), 5)
})

test_that("min_count_laa_data correctly returns data when it should", {
  minlaa_low_n_test <- min_count_laa_data(
    spp_data_low_n,
    sub_levels = "spp",
    min_total_sample_size = 30
  )
  minlaa_test <- min_count_laa_data(
    spp_data_low_n,
    sub_levels = "spp",
    min_total_sample_size = 20
  )
  minlaa_test2 <- min_count_laa_data(
    spp_data,
    sub_levels = "spp",
    min_total_sample_size = 250
  )
  minlaa_age_test <- min_count_laa_data(
    spp_data,
    sub_levels = "spp",
    min_age_sample_size = 5,
    min_age_groups = 5
  ) %>% dplyr::count(spp)
  minlaa_age_test_exp <- spp_data %>% dplyr::count(spp)
  minlaa_age_test2 <- min_count_laa_data(
    spp_data_low_age_n,
    sub_levels = "spp",
    min_age_sample_size = 1,
    min_age_groups = 5
  ) %>% dplyr::count(spp)
  minlaa_age_test2_exp <- spp_data_low_age_n %>% dplyr::count(spp)
  expect_null(minlaa_low_n_test)
  expect_equal(unique(minlaa_test$spp), c("bluegill", "walleye", "pike"))
  expect_equal(unique(minlaa_test2$spp), c("bluegill", "pike"))
  expect_equal(minlaa_age_test, minlaa_age_test_exp)
  expect_equal(minlaa_age_test2, minlaa_age_test2_exp)
})

test_that("min_age_groups correctly returns data when it should", {
  expect_equal(min_age_groups(laa_data, min_age_grps = 11), laa_data)
  expect_null(min_age_groups(laa_data, min_age_grps = 12))
  all_spp <- min_age_groups(spp_data, "spp", 9)
  no_blg <- min_age_groups(spp_data, "spp", 10)
  wae_pike <- min_age_groups(spp_data, "spp", 11)
  no_wae <- min_age_groups(spp_data, "spp", 12)
  expect_equal(unique(all_spp$spp), c("bluegill", "walleye", "pike"))
  expect_equal(unique(no_blg$spp), c("walleye", "pike"))
  expect_equal(unique(wae_pike$spp), c("walleye", "pike"))
  expect_equal(unique(no_wae$spp), c("pike"))
  expect_null(min_age_groups(spp_data, "spp", 22))
})

test_that("bin_lengths works as expected", {
  len <- c(2.1, 3.6, 4.5, 7.2, 8.1)
  lenbin2 <- c(2, 2, 4, 6, 8)
  lenbin3 <- c(0, 3, 3, 6, 6)
  lenbin4 <- c(0, 0, 4, 4, 8)
  lenbin5 <- c(0, 0, 0, 5, 5)
  expect_equal(bin_lengths(len, 2), lenbin2)
  expect_equal(bin_lengths(len, 3), lenbin3)
  expect_equal(bin_lengths(len, 4), lenbin4)
  expect_equal(bin_lengths(len, 5), lenbin5)
  lenbin_char <- paste(lenbin2, c(4, 4, 6, 8, 10), sep = "-")
  lenbin_levels <- paste(seq(0, 8, by = 2), seq(2, 10, by = 2), sep = "-")
  lenbin_char <- ordered(lenbin_char, levels = lenbin_levels)
  expect_equal(bin_lengths(len, 2, include_upper = TRUE), lenbin_char)
})

test_that("make_alk words--cursory test--see test-alk-fitting.R", {
  test_laa_data <- data.frame(
    age = c(2,2,3,3,4),
    length = c(10, 11, 12, 13, 14)
  )
  test_alk <- make_alk(
    test_laa_data,
    min_age_sample_size = 1,
    min_age_groups = 1
  )
  exp_alk <- data.frame(
    length = 10:14, age2 = c(1,1,0,0,0), age3 = c(0,0,1,1,0), age4 = c(0,0,0,0,1)
  )
  attributes(test_alk) <- attributes(exp_alk)
  expect_equal(test_alk, exp_alk)
})

test_that("rename_laa_cols returns column names as expected", {
  renamed_laa_data <- rename_laa_cols(laa_data, "do", "re", goback = TRUE)
  orig_laa_data <- rename_laa_cols(renamed_laa_data, "do", "re")
  expect_equal(names(renamed_laa_data), c("re", "do"))
  expect_equal(names(laa_data), names(orig_laa_data))
})


test_that("rename_age_col correctly specifies age columns", {
  renamed_age_data <- rename_age_col(laa_data, "foo", back = TRUE)
  orig_laa_data <- rename_age_col(renamed_age_data, "foo")
  expect_equal(names(renamed_age_data), c("foo", "length"))
  expect_equal(names(laa_data), names(orig_laa_data))
})

test_that("rename_size_col correctly specifies age columns", {
  renamed_size_data <- rename_size_col(laa_data, "foo", back = TRUE)
  orig_laa_data <- rename_size_col(renamed_size_data, "foo")
  expect_equal(names(renamed_size_data), c("age", "foo"))
  expect_equal(names(laa_data), names(orig_laa_data))
})

test_that("integral_quotient returns correct values", {
  ref_curve_params <- list(linf = 60, k = 0.25, t0 = -0.5)
  comp_curve_params <- list(linf = 62, k = 0.25, t0 = -0.4)
  comp_curve2_params <- list(linf = 65, k = 0.25, t0 = -1)
  expect_equal(
    integral_quotient(ref_curve_params, comp_curve_params, 0, 10), 2.56
  )
  expect_equal(
    integral_quotient(ref_curve_params, comp_curve2_params, 0, 10), 14.44
  )
})

