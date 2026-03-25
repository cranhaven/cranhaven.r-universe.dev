ae2_table_path <- "../data/ae2.csv"
cutoff_date <- lubridate::ymd("2023-03-18")

test_that("featurise_time_since (first,years)", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  diag_101 <- featurise(
    all_tables,
    json_to_feature("../spec/test_time_since_firstyears.json")
  )

  # Check the result
  orig_table <- read_one_table(ae2_table_path)
  diag_101_expected <- orig_table %>%
    filter(diagnosis_1 == 101 | diagnosis_2 == 101 | diagnosis_3 == 101) %>%
    mutate(
      years_since_first_101_diag =
        (cutoff_date - time) %/% lubridate::ddays(365.25)
    ) %>%
    group_by(id) %>%
    summarise(years_since_first_101_diag = max(years_since_first_101_diag)) %>%
    select(id, years_since_first_101_diag)
  for (id_num in orig_table$id) {
    if (!id_num %in% diag_101_expected$id) {
      diag_101_expected <- diag_101_expected %>%
        dplyr::add_row(id = id_num, years_since_first_101_diag = 40)
    }
  }

  expect_equal(diag_101$feature_table, diag_101_expected)
})

test_that("featurise_time_since (last,years)", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  diag_101 <- featurise(
    all_tables,
    json_to_feature("../spec/test_time_since_lastyears.json")
  )

  # Check the result
  orig_table <- read_one_table(ae2_table_path)
  diag_101_expected <- orig_table %>%
    filter(diagnosis_1 == 101 | diagnosis_2 == 101 | diagnosis_3 == 101) %>%
    mutate(
      years_since_last_101_diag =
        (cutoff_date - time) %/% lubridate::ddays(365.25)
    ) %>%
    group_by(id) %>%
    summarise(years_since_last_101_diag = min(years_since_last_101_diag)) %>%
    select(id, years_since_last_101_diag)
  for (id_num in orig_table$id) {
    if (!id_num %in% diag_101_expected$id) {
      diag_101_expected <- diag_101_expected %>%
        dplyr::add_row(id = id_num, years_since_last_101_diag = 40)
    }
  }

  expect_equal(diag_101$feature_table, diag_101_expected)
})

test_that("featurise_time_since (first,days)", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  diag_101 <- featurise(
    all_tables,
    json_to_feature("../spec/test_time_since_firstdays.json")
  )

  # Check the result
  orig_table <- read_one_table(ae2_table_path)
  diag_101_expected <- orig_table %>%
    filter(diagnosis_1 == 101 | diagnosis_2 == 101 | diagnosis_3 == 101) %>%
    mutate(
      days_since_first_101_diag =
        (cutoff_date - time) %/% lubridate::ddays(1)
    ) %>%
    group_by(id) %>%
    summarise(days_since_first_101_diag = max(days_since_first_101_diag)) %>%
    select(id, days_since_first_101_diag)
  for (id_num in orig_table$id) {
    if (!id_num %in% diag_101_expected$id) {
      diag_101_expected <- diag_101_expected %>%
        dplyr::add_row(id = id_num, days_since_first_101_diag = 40)
    }
  }

  expect_equal(diag_101$feature_table, diag_101_expected)
})
