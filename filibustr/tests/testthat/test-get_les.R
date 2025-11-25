test_that("LES 93rd-118th House data", {
  skip_if_offline()

  hr <- get_les("hr")

  # data checks
  expect_s3_class(hr, "tbl_df")
  expect_length(hr, 88)
  expect_equal(nrow(hr), 11606)
  expect_equal(unique(hr$congress), 93:118)
  # house data uses the first year of the Congress
  expect_equal(unique(hr$year), seq(1973, 2023, 2))
  # `st_name` includes non-voting members' territories
  expect_identical(levels(hr$st_name),
                   c(datasets::state.abb,
                     "AS", "DC", "GU", "MP", "PR", "VI"))

  # chamber argument
  expect_equal(hr, get_les("house"))
})

test_that("LES 93rd-118th Senate data", {
  skip_if_offline()

  sen <- get_les("s")

  # data checks
  expect_s3_class(sen, "tbl_df")
  expect_length(sen, 88)
  expect_equal(nrow(sen), 2635)
  expect_equal(unique(sen$congress), 93:118)
  # senate data uses the year of the election
  expect_equal(unique(sen$year), seq(1972, 2022, 2))
  # `state` just includes states
  expect_identical(levels(sen$state), datasets::state.abb)

  # chamber argument
  expect_equal(sen, get_les("senate"))
})

test_that("column types", {
  skip_if_offline()

  # Senate data
  sen <- get_les("sen")
  expect_s3_class(sen, "tbl_df")
  expect_length(sen, 88)
  expect_equal(nrow(sen), 2635)
  expect_length(dplyr::select(sen, dplyr::where(is.integer)), 54)
  expect_length(dplyr::select(sen, dplyr::where(is.double)), 15)
  expect_length(dplyr::select(sen, dplyr::where(is.factor)), 3)
  expect_length(dplyr::select(sen, dplyr::where(is.character)), 4)
  expect_length(dplyr::select(sen, dplyr::where(is.logical)), 12)

  # House data
  hr <- get_les("h")
  expect_s3_class(hr, "tbl_df")
  expect_length(hr, 88)
  expect_equal(nrow(hr), 11606)
  expect_length(dplyr::select(hr, dplyr::where(is.integer)), 54)
  expect_length(dplyr::select(hr, dplyr::where(is.double)), 15)
  expect_length(dplyr::select(hr, dplyr::where(is.factor)), 3)
  expect_length(dplyr::select(hr, dplyr::where(is.character)), 3)
  expect_length(dplyr::select(hr, dplyr::where(is.logical)), 13)
})

test_that("LES local reading and writing", {
  skip_if_offline()

  ## create temp file paths
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_tab <- tempfile(fileext = ".tab")
  tmp_tsv <- tempfile(fileext = ".tsv")
  tmp_dta <- tempfile(fileext = ".dta")

  ## download data from online
  s_online <- get_les("s")
  expect_s3_class(s_online, "tbl_df")
  readr::write_csv(s_online, tmp_csv)
  readr::write_tsv(s_online, tmp_tsv)

  hr_online <- get_les("hr")
  expect_s3_class(hr_online, "tbl_df")
  readr::write_tsv(hr_online, tmp_tab)
  haven::write_dta(hr_online, tmp_dta)

  ## check that local data matches
  s1_local <- get_les("s", local_path = tmp_csv)
  expect_s3_class(s1_local, "tbl_df")
  expect_equal(s1_local, haven::zap_label(haven::zap_formats(s_online)))

  h1_local <- get_les("hr", local_path = tmp_tab)
  expect_s3_class(h1_local, "tbl_df")
  expect_equal(h1_local, haven::zap_label(haven::zap_formats(hr_online)))

  s2_local <- get_les("s", local_path = tmp_tsv)
  expect_s3_class(s2_local, "tbl_df")
  expect_equal(s2_local, haven::zap_label(haven::zap_formats(s_online)))

  h2_local <- get_les("hr", local_path = tmp_dta)
  expect_s3_class(h2_local, "tbl_df")
  # don't need to zap label/formats since we saved data in a DTA file
  expect_equal(h2_local, hr_online)

  ## test that re-written data matches
  readr::write_csv(h1_local, tmp_csv)
  h1_rewritten <- get_les("hr", local_path = tmp_csv)
  expect_s3_class(h1_rewritten, "tbl_df")
  expect_equal(h1_rewritten, h1_local)

  haven::write_dta(s2_local, tmp_dta)
  s2_rewritten <- get_les("s", local_path = tmp_dta)
  expect_s3_class(s2_rewritten, "tbl_df")
  expect_equal(haven::zap_formats(s2_rewritten), s2_local)
})

test_that("LES column labels", {
  skip_if_offline()

  les_s <- get_les("sen")
  les_hr <- get_les("hr")

  # labels are non-null for all columns
  expect_length(labelled::var_label(les_s) |> purrr::keep(~ !is.null(.x)), length(les_s))
  expect_length(labelled::var_label(les_hr) |> purrr::keep(~ !is.null(.x)), length(les_hr))

  # labels are strings for all columns
  expect_identical(purrr::map_chr(labelled::var_label(les_s), typeof),
                   setNames(rep("character", length(les_s)), colnames(les_s)))
  expect_identical(purrr::map_chr(labelled::var_label(les_hr), typeof),
                   setNames(rep("character", length(les_hr)), colnames(les_hr)))

  # labels are positive-length strings
  expect_true(all(labelled::var_label(les_s) |> purrr::map_int(stringr::str_length) > 0))
  expect_true(all(labelled::var_label(les_hr) |> purrr::map_int(stringr::str_length) > 0))
})
