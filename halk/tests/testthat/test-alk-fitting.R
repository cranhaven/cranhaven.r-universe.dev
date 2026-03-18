
test_that("make_alk function works as expected", {
  lenage <- data.frame(
    age = c(0, 0, 1, 1, 2, 3, 4),
    length = c(1.5, 2.3, 3.4, 4.5, 4.6, 5.2, 6.1)
  )
  alk_normdist <-
    lenage %>%
    dplyr::group_by(age) %>%
    dplyr::summarize(
      mean = mean(length, na.rm = TRUE),
      sd = sd(length, na.rm = TRUE)
    ) %>%
    dplyr::mutate(sd = dplyr::case_when(
      sd == 0 ~ 0.3,
      is.na(sd) ~ 0.3,
      TRUE ~ sd
    ))
  exp_alk <- tibble::tibble(
    length = 1:6,
    `age0` = c(1, 1, rep(0, 4)),
    `age1` = c(0, 0, 1, 0.5, 0, 0),
    `age2` = c(rep(0, 3), 0.5, 0, 0),
    `age3` = c(rep(0, 4), 1, 0),
    `age4` = c(rep(0, 5), 1)
  )
  attr(exp_alk, "size_col") <- "length"
  attr(exp_alk, "age_col") <- "age"
  attr(exp_alk, "autobin") <- TRUE
  attr(exp_alk, "size_bin") <- 1
  attr(exp_alk, "alk_n") <- nrow(lenage)
  attr(exp_alk, "dnorm_params") <- alk_normdist
  alk_normdist <-
    lenage %>%
    dplyr::group_by(age) %>%
    dplyr::summarize(
      mean = mean(length, na.rm = TRUE),
      sd = sd(length, na.rm = TRUE)
    ) %>%
    dplyr::mutate(sd = dplyr::case_when(
      sd == 0 ~ 0.3,
      is.na(sd) ~ 0.3,
      TRUE ~ sd
    ))
  attr(exp_alk, "dnorm_params") <- alk_normdist
  class(exp_alk) <- c("alk", class(exp_alk))
  expect_equal(make_alk(lenage, min_age_sample_size = 1), exp_alk)
})

assign_attributes_test <- function(data, alk_class = "halk_fit", levels=NULL) {
  attr(data, "size_col") <- "length"
  attr(data, "age_col") <- "age"
  # attr(data, "autobin") <- TRUE
  # attr(data, "size_bin") <- 1
  if (!is.null(levels)) {
    attr(data, "levels") <- levels
  }
  class(data)  <- c(alk_class, class(data))
  return(data)
}

test_that("make_alk works correctly", {
  exp_alk <-
    make_alk(laa_data) %>%
    assign_attributes_test(alk_class = NULL)
  exp_spp_alk <-
    spp_data %>%
    dplyr::group_by(spp) %>%
    tidyr::nest() %>%
    dplyr::summarize(alk = purrr::map(data, make_alk)) %>%
    assign_attributes_test(levels = "spp")
  exp_wb_spp_alk <-
    wb_spp_data %>%
    dplyr::group_by(spp, county, waterbody) %>%
    tidyr::nest() %>%
    dplyr::mutate(alk = purrr::map(data, make_alk)) %>%
    dplyr::select(-data)
  exp_county_spp_alk <-
    wb_spp_data %>%
    dplyr::group_by(spp, county) %>%
    tidyr::nest() %>%
    dplyr::mutate(alk = purrr::map(data, make_alk)) %>%
    dplyr::select(-data)
  exp_all_spp_alk <-
    wb_spp_data %>%
    dplyr::group_by(spp) %>%
    tidyr::nest() %>%
    dplyr::mutate(alk = purrr::map(data, make_alk)) %>%
    dplyr::select(-data)
  exp_all_level_spp_alk <-
    dplyr::bind_rows(exp_wb_spp_alk, exp_county_spp_alk, exp_all_spp_alk) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(spp, county, waterbody) %>%
    assign_attributes_test(levels = c("spp", "county", "waterbody"))
  test_alk <- make_alk(laa_data)
  test_spp_alk <- make_halk(
    spp_data,
    levels = "spp",
    min_age_sample_size = 1,
    min_total_sample_size = 1
  )
  test_wb_spp_alk <- make_halk(
    wb_spp_data,
    levels = c("spp", "county", "waterbody"),
    min_age_sample_size = 1,
    min_total_sample_size = 1
  )
  expect_equal(test_alk, exp_alk)
  expect_equal(test_spp_alk, exp_spp_alk)
  expect_equal(test_wb_spp_alk, exp_all_level_spp_alk)
})



