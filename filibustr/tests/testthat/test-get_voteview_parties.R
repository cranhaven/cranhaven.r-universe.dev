test_that("all parties data", {
  skip_if_offline()

  all_parties <- get_voteview_parties()
  expect_s3_class(all_parties, "tbl_df")
  expect_length(all_parties, 9)
  expect_equal(levels(all_parties$chamber),
               c("President", "House", "Senate"))
  # allow Congresses to be 1:(current_congress() - 1) in January of odd years
  # since Voteview may not have votes from the new Congress yet
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(all_parties$congress), 1:current_congress()) ||
                  all.equal(unique(all_parties$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(all_parties$congress), 1:current_congress())
  }
})

test_that("filter parties by congress", {
  skip_if_offline()

  parties_99_101 <- get_voteview_parties(congress = 99:101)
  expect_s3_class(parties_99_101, "tbl_df")
  expect_length(parties_99_101, 9)
  expect_equal(nrow(parties_99_101), 15)
  expect_equal(unique(parties_99_101$congress), 99:101)
  expect_equal(levels(parties_99_101$party_name),
               c("Republican", "Democrat"))

  parties_117 <- get_voteview_parties(congress = 117)
  expect_s3_class(parties_117, "tbl_df")
  expect_length(parties_117, 9)
  expect_equal(nrow(parties_117), 7)
  expect_equal(unique(parties_117$congress), 117)
  expect_equal(levels(parties_117$party_name),
               c("Democrat", "Republican", "Independent"))
})

test_that("parties column types", {
  skip_if_offline()

  parties_25 <- get_voteview_parties(congress = 25)
  expect_s3_class(parties_25, "tbl_df")
  expect_length(parties_25, 9)
  expect_equal(nrow(parties_25), 8)

  expect_length(dplyr::select(parties_25, dplyr::where(is.double)), 4)
  expect_length(dplyr::select(parties_25, dplyr::where(is.integer)), 3)
  expect_length(dplyr::select(parties_25, dplyr::where(is.factor)), 2)
})

test_that("local read/write", {
  skip_if_offline()

  ## create filepaths
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_tsv <- tempfile(fileext = ".tsv")
  tmp_dta <- tempfile(fileext = ".dta")

  ## download online data
  all_parties_online <- get_voteview_parties()
  expect_s3_class(all_parties_online, "tbl_df")
  expect_length(all_parties_online, 9)
  # this table will grow in each congress
  expect_gte(nrow(all_parties_online), 845)
  expect_equal(levels(all_parties_online$chamber), c("President", "House", "Senate"))
  readr::write_tsv(all_parties_online, tmp_tsv)

  s_100_parties_online <- get_voteview_parties(chamber = "s", congress = 100)
  expect_s3_class(s_100_parties_online, "tbl_df")
  expect_length(s_100_parties_online, 9)
  expect_gte(nrow(s_100_parties_online), 3)
  expect_equal(levels(all_parties_online$chamber), c("President", "House", "Senate"))
  readr::write_csv(s_100_parties_online, tmp_csv)

  ## check that local data matches
  all_parties_local <- get_voteview_parties(local_path = tmp_tsv)
  expect_s3_class(all_parties_local, "tbl_df")
  expect_equal(all_parties_local, all_parties_online)

  s_100_parties_local <- get_voteview_parties(chamber = "s", congress = 100,
                                              local_path = tmp_csv)
  expect_s3_class(s_100_parties_local, "tbl_df")
  expect_equal(s_100_parties_local, s_100_parties_online)
})

test_that("local read filtering", {
  skip_if_offline()

  ## create filepath
  tmp_dta <- tempfile(fileext = ".dta")
  tmp_csv <- tempfile(fileext = ".csv")

  ## download and save from online
  all_parties <- get_voteview_parties()
  expect_s3_class(all_parties, "tbl_df")
  expect_length(all_parties, 9)
  expect_equal(nrow(all_parties), 845)
  haven::write_dta(all_parties, tmp_dta)
  readr::write_csv(all_parties, tmp_csv)

  ## read whole file from local
  local_parties <- get_voteview_parties(local_path = tmp_dta)
  expect_s3_class(local_parties, "tbl_df")
  expect_equal(haven::zap_labels(haven::zap_formats(local_parties)), all_parties)

  local_parties2 <- get_voteview_parties(local_path = tmp_csv)
  expect_s3_class(local_parties2, "tbl_df")
  expect_equal(local_parties2, all_parties)

  ## filter by chamber
  hr_parties <- get_voteview_parties(chamber = "hr",
                                     local_path = tmp_dta)
  expect_s3_class(hr_parties, "tbl_df")
  expect_equal(nrow(hr_parties), 521)
  expect_equal(haven::zap_formats(hr_parties),
               dplyr::filter(all_parties, chamber != "Senate") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  ## filter by congress
  parties_50s <- get_voteview_parties(congress = 50:59,
                                      local_path = tmp_csv)
  expect_equal(nrow(parties_50s), 83)
  expect_equal(parties_50s, dplyr::filter(all_parties, congress %in% 50:59))

  ## filter by both
  s_parties_117 <- get_voteview_parties(chamber = "sen", congress = 117,
                                        local_path = tmp_dta)
  expect_s3_class(s_parties_117, "tbl_df")
  expect_equal(nrow(s_parties_117), 5)
  expect_equal(haven::zap_labels(haven::zap_formats(s_parties_117)),
               dplyr::filter(all_parties,
                             chamber != "House",
                             congress == 117) |>
                 dplyr::mutate(chamber = droplevels(chamber)))
})
