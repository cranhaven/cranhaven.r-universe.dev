test_that("HVW house data", {
  skip_if_offline()

  house_data <- get_hvw_data("house")

  # data checks
  expect_s3_class(house_data, "tbl_df")
  expect_length(house_data, 109)
  expect_equal(nrow(house_data), 9825)
  expect_equal(unique(house_data$congress), 93:114)
  expect_equal(unique(house_data$year), c(seq(1973, 2015, 2), NA))

  # check chamber argument
  expect_equal(house_data, get_hvw_data("h"))
  expect_equal(house_data, get_hvw_data("hr"))
})

test_that("HVW senate data", {
  skip_if_offline()

  senate_data <- get_hvw_data("senate")

  # data checks
  expect_s3_class(senate_data, "tbl_df")
  expect_length(senate_data, 104)
  expect_equal(nrow(senate_data), 2228)
  expect_equal(unique(senate_data$congress), 93:114)
  expect_equal(unique(senate_data$year), seq(1972, 2014, 2))

  # check chamber argument
  expect_equal(senate_data, get_hvw_data("s"))
  expect_equal(senate_data, get_hvw_data("sen"))
})

test_that("HVW chamber errors", {
  expect_error(get_hvw_data("all"))
  expect_error(get_hvw_data("congress"))
  expect_error(get_hvw_data(), "argument \"chamber\" is missing, with no default")
})

test_that("HVW local reading and writing", {
  skip_if_offline()

  ## create temp file paths
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_tsv <- tempfile(fileext = ".tsv")
  tmp_tab <- tempfile(fileext = ".tab")

  ## download data from online
  sen_online <- get_hvw_data(chamber = "s")
  expect_s3_class(sen_online, "tbl_df")
  expect_length(sen_online, 104)
  expect_equal(nrow(sen_online), 2228)
  expect_equal(unique(sen_online$congress), 93:114)
  expect_equal(unique(sen_online$year), seq(1972, 2014, 2))
  readr::write_csv(sen_online, tmp_csv)

  hr_online <- get_hvw_data(chamber = "hr")
  expect_s3_class(hr_online, "tbl_df")
  expect_length(hr_online, 109)
  expect_equal(nrow(hr_online), 9825)
  expect_equal(unique(hr_online$congress), 93:114)
  expect_equal(unique(hr_online$year), c(seq(1973, 2015, 2), NA))
  readr::write_tsv(hr_online, tmp_tsv)

  ## check that local data matches
  sen_local <- get_hvw_data("s", local_path = tmp_csv)
  expect_s3_class(sen_local, "tbl_df")
  expect_equal(nrow(sen_local), 2228)
  expect_equal(sen_online, sen_local)
  readr::write_tsv(sen_local, tmp_tab)

  hr_local <- get_hvw_data("hr", local_path = tmp_tsv)
  expect_s3_class(hr_local, "tbl_df")
  expect_equal(hr_online, hr_local)
  readr::write_tsv(hr_local, tmp_tsv)

  ## test that re-written data matches
  sen_rewritten <- get_hvw_data("s", local_path = tmp_tab)
  expect_equal(sen_online, sen_rewritten)

  # writing house data in previous senate file
  readr::write_csv(hr_local, tmp_csv)
  hr_in_sen_file <- get_hvw_data("hr", local_path = tmp_csv)
  expect_equal(hr_online, hr_in_sen_file)
})

test_that("HVW data with Stata dta files", {
  skip_if_offline()

  tmp_dta <- tempfile(fileext = ".dta")

  sen_to_dta <- get_hvw_data("s")
  expect_s3_class(sen_to_dta, "tbl_df")
  expect_length(sen_to_dta, 104)
  expect_equal(nrow(sen_to_dta), 2228)
  expect_equal(unique(sen_to_dta$congress), 93:114)
  expect_equal(unique(sen_to_dta$year), seq(1972, 2014, 2))
  expect_equal(as.character(sort(unique(sen_to_dta$state))),
               state.abb)
  haven::write_dta(sen_to_dta, tmp_dta)

  # Issues to check for:
  # - `state` column reading from dta as null
  # - dta formats
  sen_from_dta <- get_hvw_data("s", local_path = tmp_dta)
  expect_s3_class(sen_from_dta, "tbl_df")
  expect_length(sen_from_dta, 104)
  expect_equal(nrow(sen_from_dta), 2228)
  expect_equal(unique(sen_from_dta$congress), 93:114)
  expect_equal(unique(sen_from_dta$year), seq(1972, 2014, 2))
  expect_equal(as.character(sort(unique(sen_from_dta$state))),
               state.abb)
  expect_equal(sen_to_dta, haven::zap_formats(sen_from_dta))
})

test_that("basic dta example", {
  skip_if_offline()

  tmp_dta <- tempfile(fileext = ".dta")

  # read data from online
  from_online <- readr::read_tsv("https://dataverse.harvard.edu/api/access/datafile/6299605",
                                 show_col_types = FALSE)
  expect_s3_class(from_online, "tbl_df")
  expect_length(from_online, 104)
  expect_equal(nrow(from_online), 2228)
  # `state` column uses character type
  expect_equal(sort(unique(from_online$state)),
               sort(state.abb))

  # write to DTA file
  haven::write_dta(from_online, tmp_dta)

  # read from the DTA file
  from_dta <- haven::read_dta(tmp_dta)
  expect_s3_class(from_dta, "tbl_df")
  # `state` column is correct
  expect_equal(sort(unique(from_dta$state)),
               sort(state.abb))

  ## convert `state` column to factor
  from_online <- from_online |>
    dplyr::mutate(dplyr::across(.cols = "state",
                                .fns = ~ factor(.x, levels = datasets::state.abb)))
  # `state` is still correct (matches `state.abb`)
  expect_equal(as.character(sort(unique(from_online$state))),
               state.abb)

  # write to DTA with `state` as a factor
  haven::write_dta(from_online, tmp_dta)

  # read from the DTA file
  from_dta <- haven::read_dta(tmp_dta) |>
    dplyr::mutate(dplyr::across(.cols = state,
                                .fns = haven::as_factor))
  expect_s3_class(from_dta, "tbl_df")
  # `state` column is correct
  expect_equal(as.character(sort(unique(from_dta$state))),
               state.abb)
})

test_that("HVW House data includes non-voting members", {
  skip_if_offline()

  hvw_hr <- get_hvw_data("hr")

  expect_s3_class(hvw_hr$st_name, "factor")
  expect_length(levels(hvw_hr$st_name), 56)
  expect_identical(levels(hvw_hr$st_name), c(state.abb,
                                             "AS", "DC", "GU", "MP", "PR", "VI"))
  expect_contains(hvw_hr$st_name, c("AS", "DC", "GU", "MP", "PR", "VI"))
})
