test_that("download from Voteview", {
  skip_if_offline()

  online_rollcalls <- get_voteview_rollcall_votes()
  expect_s3_class(online_rollcalls, "tbl_df")
  expect_length(online_rollcalls, 18)
  expect_equal(levels(online_rollcalls$chamber), c("House", "Senate"))
  # allow Congresses to be 1:(current_congress() - 1) in January of odd years
  # since Voteview may not have votes from the new Congress yet
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(online_rollcalls$congress), 1:current_congress()) ||
                  all.equal(unique(online_rollcalls$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(online_rollcalls$congress), 1:current_congress())
  }
})

test_that("filter rollcalls by chamber", {
  skip_if_offline()

  s_votes <- get_voteview_rollcall_votes(chamber = "s")
  expect_s3_class(s_votes, "tbl_df")
  expect_length(s_votes, 18)
  expect_equal(levels(s_votes$chamber), "Senate")
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(s_votes$congress), 1:current_congress()) ||
                  all.equal(unique(s_votes$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(s_votes$congress), 1:current_congress())
  }

  hr_votes <- get_voteview_rollcall_votes(chamber = "hr")
  expect_s3_class(hr_votes, "tbl_df")
  expect_length(hr_votes, 18)
  expect_equal(levels(hr_votes$chamber), "House")
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(hr_votes$congress), 1:current_congress()) ||
                  all.equal(unique(hr_votes$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(hr_votes$congress), 1:current_congress())
  }

  # House has more recorded votes
  expect_gt(nrow(hr_votes), nrow(s_votes))
})

test_that("filter rollcalls by congress", {
  skip_if_offline()

  rollcalls_1_10 <- get_voteview_rollcall_votes(congress = 1:10)
  expect_s3_class(rollcalls_1_10, "tbl_df")
  expect_length(rollcalls_1_10, 18)
  expect_equal(levels(rollcalls_1_10$chamber), c("House", "Senate"))
  expect_equal(unique(rollcalls_1_10$congress), 1:10)
  expect_equal(nrow(rollcalls_1_10), 2339)

  rollcalls_101_110 <- get_voteview_rollcall_votes(congress = 101:110)
  expect_s3_class(rollcalls_101_110, "tbl_df")
  expect_length(rollcalls_101_110, 18)
  expect_equal(levels(rollcalls_101_110$chamber), c("House", "Senate"))
  expect_equal(unique(rollcalls_101_110$congress), 101:110)
  expect_equal(nrow(rollcalls_101_110), 18578)

  # combo of chamber and congress
  s_rollcalls_117 <- get_voteview_rollcall_votes(chamber = "s", congress = 117)
  expect_s3_class(s_rollcalls_117, "tbl_df")
  expect_length(s_rollcalls_117, 18)
  expect_equal(levels(s_rollcalls_117$chamber), "Senate")
  expect_equal(unique(s_rollcalls_117$congress), 117)
  expect_equal(nrow(s_rollcalls_117), 949)
})

test_that("rollcalls column types", {
  skip_if_offline()

  hr_votes_31 <- get_voteview_rollcall_votes(chamber = "hr", congress = 31)
  expect_s3_class(hr_votes_31, "tbl_df")
  expect_length(hr_votes_31, 18)
  expect_equal(nrow(hr_votes_31), 572)

  expect_length(dplyr::select(hr_votes_31, dplyr::where(is.character)), 5)
  expect_length(dplyr::select(hr_votes_31, dplyr::where(is.double)), 6)
  expect_length(dplyr::select(hr_votes_31, dplyr::where(is.factor)), 1)
  expect_length(dplyr::select(hr_votes_31, dplyr::where(is.integer)), 6)

  votes_109 <- get_voteview_rollcall_votes(congress = 109)
  expect_s3_class(votes_109, "tbl_df")
  expect_length(votes_109, 18)
  expect_equal(nrow(votes_109), 1855)

  expect_length(dplyr::select(votes_109, dplyr::where(is.character)), 5)
  expect_length(dplyr::select(votes_109, dplyr::where(is.double)), 6)
  expect_length(dplyr::select(votes_109, dplyr::where(is.factor)), 1)
  expect_length(dplyr::select(votes_109, dplyr::where(is.integer)), 6)
})

test_that("local read/write", {
  skip_if_offline()

  ## create filepaths
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_tsv <- tempfile(fileext = ".tsv")
  tmp_dta <- tempfile(fileext = ".dta")

  ## download online data
  online_rollcalls_110_113 <- get_voteview_rollcall_votes(congress = 110:113)
  expect_s3_class(online_rollcalls_110_113, "tbl_df")
  expect_length(online_rollcalls_110_113, 18)
  expect_equal(nrow(online_rollcalls_110_113), 8812)
  expect_identical(levels(online_rollcalls_110_113$chamber), c("House", "Senate"))
  haven::write_dta(online_rollcalls_110_113, tmp_dta)

  online_rollcalls_current <- get_voteview_rollcall_votes(congress = 119:current_congress())
  expect_s3_class(online_rollcalls_current, "tbl_df")
  expect_length(online_rollcalls_current, 18)
  # this table will grow over time
  expect_gt(nrow(online_rollcalls_current), 53)
  readr::write_csv(online_rollcalls_current, tmp_csv)

  ## check that local data matches
  local_rollcalls_110_113 <- get_voteview_rollcall_votes(local_path = tmp_dta)
  expect_s3_class(local_rollcalls_110_113, "tbl_df")
  expect_identical(haven::zap_labels(haven::zap_formats(local_rollcalls_110_113)),
                   online_rollcalls_110_113)

  local_rollcalls_current <- get_voteview_rollcall_votes(local_path = tmp_csv)
  expect_s3_class(local_rollcalls_110_113, "tbl_df")
  expect_identical(local_rollcalls_current,
                   online_rollcalls_current)
})

test_that("local read filtering", {
  skip_if_offline()

  ## create filepaths
  tmp_dta <- tempfile(fileext = ".dta")
  tmp_tsv <- tempfile(fileext = ".tsv")

  ## download and save from online
  online_rolcalls_80s <- get_voteview_rollcall_votes(congress = 80:89)
  expect_s3_class(online_rolcalls_80s, "tbl_df")
  expect_length(online_rolcalls_80s, 18)
  expect_equal(nrow(online_rolcalls_80s), 5846)
  haven::write_dta(online_rolcalls_80s, tmp_dta)
  # don't double the quotes when saving to TSV
  readr::write_tsv(online_rolcalls_80s, tmp_tsv, escape = "none")

  ## read whole file from local
  local_rollcalls_1 <- get_voteview_rollcall_votes(local_path = tmp_tsv)
  expect_s3_class(local_rollcalls_1, "tbl_df")
  expect_identical(local_rollcalls_1, online_rolcalls_80s)

  local_rollcalls_2 <- get_voteview_rollcall_votes(local_path = tmp_dta)
  expect_s3_class(local_rollcalls_2, "tbl_df")
  expect_identical(haven::zap_labels(haven::zap_formats(local_rollcalls_2)),
                   online_rolcalls_80s)

  ## filter by chamber
  sen_rollcalls_1 <- get_voteview_rollcall_votes(chamber = "s", local_path = tmp_tsv)
  expect_s3_class(sen_rollcalls_1, "tbl_df")
  expect_equal(nrow(sen_rollcalls_1), 3692)
  expect_equal(sen_rollcalls_1,
               online_rolcalls_80s |>
                 dplyr::filter(chamber == "Senate") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  hr_rollcalls_2 <- get_voteview_rollcall_votes(chamber = "house", local_path = tmp_dta)
  expect_s3_class(hr_rollcalls_2, "tbl_df")
  expect_equal(nrow(hr_rollcalls_2), 2154)
  expect_equal(haven::zap_formats(hr_rollcalls_2),
               online_rolcalls_80s |>
                 dplyr::filter(chamber == "House") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  ## filter by congress
  rollcalls_82 <- get_voteview_rollcall_votes(congress = 82, local_path = tmp_dta)
  expect_s3_class(rollcalls_82, "tbl_df")
  expect_equal(nrow(rollcalls_82), 494)
  expect_equal(haven::zap_formats(rollcalls_82),
               online_rolcalls_80s |>
                 dplyr::filter(congress == 82))


  rollcalls_87_and_89 <- get_voteview_rollcall_votes(congress = c(87, 89),
                                                     local_path = tmp_dta)
  expect_s3_class(rollcalls_87_and_89, "tbl_df")
  expect_equal(nrow(rollcalls_87_and_89), 1559)
  expect_equal(haven::zap_formats(rollcalls_87_and_89),
               online_rolcalls_80s |>
                 dplyr::filter(congress %in% c(87, 89)))

  ## filter by both
  sen_rollcalls_85_87 <- get_voteview_rollcall_votes(chamber = "senate",
                                                     congress = 85:87,
                                                     local_path = tmp_tsv)
  expect_s3_class(sen_rollcalls_85_87, "tbl_df")
  expect_equal(nrow(sen_rollcalls_85_87), 1157)
  expect_equal(haven::zap_formats(sen_rollcalls_85_87),
               online_rolcalls_80s |>
                 dplyr::filter(congress %in% c(85, 86, 87),
                               chamber == "Senate") |>
                 dplyr::mutate(chamber = droplevels(chamber)))
})
