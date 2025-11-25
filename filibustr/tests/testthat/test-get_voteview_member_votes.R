test_that("download from Voteview", {
  skip("Skipping slow online member-votes downloads.")
  skip_if_offline()

  online_votes <- get_voteview_member_votes()
  expect_s3_class(online_votes, "tbl_df")
  expect_length(online_votes, 6)
  expect_equal(levels(online_votes$chamber), c("House", "Senate"))
  # allow Congresses to be 1:(current_congress() - 1) in January of odd years
  # since Voteview may not have votes from the new Congress yet
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(online_votes$congress), 1:current_congress()) ||
                  all.equal(unique(online_votes$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(online_votes$congress), 1:current_congress())
  }
  expect_gte(min(online_votes$prob, na.rm = TRUE), 0)
  expect_lte(max(online_votes$prob, na.rm = TRUE), 100)
})

test_that("filter votes by chamber", {
  skip_if_offline()

  s_votes <- get_voteview_member_votes(chamber = "s")
  expect_s3_class(s_votes, "tbl_df")
  expect_length(s_votes, 6)
  expect_equal(levels(s_votes$chamber), "Senate")
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(s_votes$congress), 1:current_congress()) ||
                  all.equal(unique(s_votes$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(s_votes$congress), 1:current_congress())
  }

  skip("Skipping slow online member-votes downloads.")

  hr_votes <- get_voteview_member_votes(chamber = "hr")
  expect_s3_class(hr_votes, "tbl_df")
  expect_length(hr_votes, 6)
  expect_equal(levels(hr_votes$chamber), "House")
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(hr_votes$congress), 1:current_congress()) ||
                  all.equal(unique(hr_votes$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(hr_votes$congress), 1:current_congress())
  }

  # 4x more members per House vote, so this will hold
  expect_gt(nrow(hr_votes), nrow(s_votes))
})

test_that("filter votes by congress", {
  skip_if_offline()

  votes_1_5 <- get_voteview_member_votes(congress = 1:5)
  expect_s3_class(votes_1_5, "tbl_df")
  expect_length(votes_1_5, 6)
  expect_equal(levels(votes_1_5$chamber), c("House", "Senate"))
  expect_equal(unique(votes_1_5$congress), 1:5)
  expect_equal(nrow(votes_1_5), 62037)

  votes_108_110 <- get_voteview_member_votes(congress = 108:110)
  expect_s3_class(votes_108_110, "tbl_df")
  expect_length(votes_108_110, 6)
  expect_equal(levels(votes_108_110$chamber), c("House", "Senate"))
  expect_equal(unique(votes_108_110$congress), 108:110)
  expect_equal(nrow(votes_108_110), 2064859)

  # combo of chamber and congress
  s_votes_117 <- get_voteview_member_votes(chamber = "s", congress = 117)
  expect_s3_class(s_votes_117, "tbl_df")
  expect_length(s_votes_117, 6)
  expect_equal(levels(s_votes_117$chamber), "Senate")
  expect_equal(unique(s_votes_117$congress), 117)
  expect_equal(nrow(s_votes_117), 95152)
})

test_that("column types", {
  skip_if_offline()

  mem_votes_100 <- get_voteview_member_votes(congress = 100)
  expect_s3_class(mem_votes_100, "tbl_df")
  expect_length(mem_votes_100, 6)
  expect_equal(nrow(mem_votes_100), 487335)

  expect_type(mem_votes_100$congress, "integer")
  expect_s3_class(mem_votes_100$chamber, "factor")
  expect_type(mem_votes_100$rollnumber, "integer")
  expect_type(mem_votes_100$icpsr, "integer")
  expect_type(mem_votes_100$cast_code, "integer")
  expect_type(mem_votes_100$prob, "double")
})

test_that("local read/write", {
  skip_if_offline()

  # create filepaths
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_tsv <- tempfile(fileext = ".tsv")
  tmp_dta <- tempfile(fileext = ".dta")

  # download online data
  sen_mvotes_104 <- get_voteview_member_votes(chamber = "s", congress = 104)
  expect_s3_class(sen_mvotes_104, "tbl_df")
  expect_length(sen_mvotes_104, 6)
  expect_equal(nrow(sen_mvotes_104), 92677)
  expect_equal(levels(sen_mvotes_104$chamber), "Senate")

  haven::write_dta(sen_mvotes_104, tmp_dta)

  all_mvotes_103_104 <- get_voteview_member_votes(chamber = "all", congress = 103:104)
  expect_s3_class(all_mvotes_103_104, "tbl_df")
  expect_length(all_mvotes_103_104, 6)
  expect_equal(nrow(all_mvotes_103_104), 1216398)
  expect_equal(levels(all_mvotes_103_104$chamber), c("House", "Senate"))

  readr::write_csv(all_mvotes_103_104, tmp_csv)

  # check that local data matches
  local_sen_mvotes_104 <- get_voteview_member_votes(chamber = "s", congress = 104,
                                                    local_path = tmp_dta)
  expect_s3_class(local_sen_mvotes_104, "tbl_df")
  # dta vs csv
  expect_identical(haven::zap_formats(local_sen_mvotes_104), sen_mvotes_104)

  csv_sen_mvotes_104 <- get_voteview_member_votes(chamber = "s", congress = 104,
                                                  local_path = tmp_csv)
  expect_s3_class(csv_sen_mvotes_104, "tbl_df")
  # csv vs online
  expect_identical(csv_sen_mvotes_104, sen_mvotes_104)
})

test_that("local read filtering", {
  skip_if_offline()

  ## create filepath
  tmp_csv <- tempfile(fileext = ".csv")

  ## download and save from online
  memvotes_20_22 <- get_voteview_member_votes(congress = 20:22)
  expect_s3_class(memvotes_20_22, "tbl_df")
  expect_length(memvotes_20_22, 6)
  expect_equal(nrow(memvotes_20_22), 250526)
  readr::write_csv(memvotes_20_22, tmp_csv)

  ## read whole file from local
  local_memvotes_20_22 <- get_voteview_member_votes(congress = 20:22, local_path = tmp_csv)
  expect_s3_class(local_memvotes_20_22, "tbl_df")
  expect_equal(local_memvotes_20_22, memvotes_20_22)

  ## filter by chamber
  hr_memvotes_20_22 <- get_voteview_member_votes(congress = 20:22, chamber = "hr",
                                                 local_path = tmp_csv)
  expect_s3_class(hr_memvotes_20_22, "tbl_df")
  expect_equal(nrow(hr_memvotes_20_22), 205790)
  expect_equal(hr_memvotes_20_22,
               dplyr::filter(memvotes_20_22, chamber != "Senate") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  # shouldn't need `congress` arg for this file
  hr_memvotes_20_22_v2 <- get_voteview_member_votes(chamber = "hr",
                                                    local_path = tmp_csv)
  expect_equal(hr_memvotes_20_22_v2, hr_memvotes_20_22)

  ## filter by congress
  memvotes_21 <- get_voteview_member_votes(chamber = "all", congress = 21,
                                           local_path = tmp_csv)
  expect_equal(nrow(memvotes_21), 71001)
  expect_equal(memvotes_21, dplyr::filter(memvotes_20_22, congress == 21))

  ## filter by both
  s_memvotes_21_22 <- get_voteview_member_votes(chamber = "sen", congress = 21:22,
                                                local_path = tmp_csv)
  expect_s3_class(s_memvotes_21_22, "tbl_df")
  expect_equal(nrow(s_memvotes_21_22), 33396)
  expect_equal(s_memvotes_21_22,
               dplyr::filter(memvotes_20_22,
                             chamber != "House",
                             congress %in% 21:22) |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  ## passing congress numbers not in the file
  # at least one congress is present: ok
  memvotes_20_not10s <- get_voteview_member_votes(congress = 10:20, local_path = tmp_csv)
  expect_s3_class(memvotes_20_not10s, "tbl_df")
  expect_equal(nrow(memvotes_20_not10s), 60954)

  # none present: error
  expect_error(get_voteview_member_votes(congress = 10:19, local_path = tmp_csv),
               "Congress numbers .+ were not found in data")
})
