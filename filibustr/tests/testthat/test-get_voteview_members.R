test_that("download from Voteview", {
  skip_if_offline()

  online_voteview_members <- get_voteview_members()
  expect_s3_class(online_voteview_members, "tbl_df")
  expect_length(online_voteview_members, 22)
  # floor on all-time members length
  expect_gt(nrow(online_voteview_members), 50400)
  expect_equal(levels(online_voteview_members$chamber),
               c("President", "House", "Senate"))
  # allow Congresses to be 1:(current_congress() - 1) in January of odd years
  # since Voteview may not have votes from the new Congress yet
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(online_voteview_members$congress), 1:current_congress()) ||
                  all.equal(unique(online_voteview_members$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(online_voteview_members$congress), 1:current_congress())
  }
})

test_that("filter by chamber", {
  skip_if_offline()

  hr <- get_voteview_members(chamber = "house")
  expect_length(hr, 22)
  expect_equal(levels(hr$chamber), c("President", "House"))
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(hr$congress), 1:current_congress()) ||
                  all.equal(unique(hr$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(hr$congress), 1:current_congress())
  }

  s <- get_voteview_members(chamber = "senate")
  expect_length(s, 22)
  expect_equal(levels(s$chamber), c("President", "Senate"))
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(s$congress), 1:current_congress()) ||
                  all.equal(unique(s$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(s$congress), 1:current_congress())
  }

  expect_gt(nrow(hr), nrow(s))

  # TODO: how to correctly test for a warning? This still produces a warning in the test.
  # expect_warning(get_voteview_members(chamber = "not a chamber"),
  #                "Invalid `chamber` argument \\(\"not a chamber\"\\) provided\\. Using `chamber = \"all\"`\\.")
})

test_that("filter by congress", {
  skip_if_offline()

  members_110 <- get_voteview_members(congress = 110, chamber = "s")
  expect_s3_class(members_110, "tbl_df")
  expect_equal(levels(members_110$chamber),
               c("President", "Senate"))
  expect_equal(unique(members_110$congress), 110)

  members_1 <- get_voteview_members(congress = 1)
  expect_s3_class(members_1, "tbl_df")
  expect_equal(levels(members_1$chamber),
               c("President", "House", "Senate"))
  expect_equal(unique(members_1$congress), 1)

  sens_all_congresses <- get_voteview_members(congress = NULL, chamber = "sen")
  expect_s3_class(sens_all_congresses, "tbl_df")
  expect_equal(levels(sens_all_congresses$chamber),
               c("President", "Senate"))
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(sens_all_congresses$congress), 1:current_congress()) ||
                  all.equal(unique(sens_all_congresses$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(sens_all_congresses$congress), 1:current_congress())
  }

  expect_gt(nrow(sens_all_congresses), nrow(members_110))

  members_90_95 <- get_voteview_members(congress = 90:95)
  expect_s3_class(members_90_95, "tbl_df")
  expect_equal(unique(members_90_95$congress), 90:95)
  expect_equal(levels(members_90_95$chamber),
               c("President", "House", "Senate"))
  expect_equal(nrow(members_90_95), 3276)

  # invalid `congress`
  expect_error(get_voteview_members(congress = 200), "Invalid `congress` argument")
})

test_that("column types", {
  skip_if_offline()

  members_98 <- get_voteview_members(congress = 98)
  expect_s3_class(members_98, "tbl_df")
  expect_equal(nrow(members_98), 542)
  expect_length(members_98, 22)

  expect_length(dplyr::select(members_98, dplyr::where(is.double)), 6)
  expect_length(dplyr::select(members_98, dplyr::where(is.integer)), 11)
  expect_length(dplyr::select(members_98, dplyr::where(is.character)), 2)
  expect_length(dplyr::select(members_98, dplyr::where(is.factor)), 2)
  expect_length(dplyr::select(members_98, dplyr::where(is.logical)), 1)
})

test_that("local read/write", {
  skip_if_offline()

  # create filepaths
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_tsv <- tempfile(fileext = ".tsv")
  tmp_dta <- tempfile(fileext = ".dta")

  # download data from online
  all_members_online <- get_voteview_members(chamber = "all")
  expect_s3_class(all_members_online, "tbl_df")
  expect_length(all_members_online, 22)
  expect_gt(nrow(all_members_online), 50400)
  readr::write_csv(all_members_online, tmp_csv)

  hr_117_online <- get_voteview_members(chamber = "hr", congress = 117)
  expect_s3_class(hr_117_online, "tbl_df")
  expect_length(hr_117_online, 22)
  expect_equal(nrow(hr_117_online), 457)
  haven::write_dta(hr_117_online, tmp_dta)

  # check that local data matches
  all_members_local <- get_voteview_members(chamber = "all", local_path = tmp_csv)
  expect_s3_class(all_members_local, "tbl_df")
  expect_equal(all_members_local, all_members_online)

  hr_117_local <- get_voteview_members(chamber = "hr", congress = 117,
                                       local_path = tmp_dta)
  expect_s3_class(hr_117_local, "tbl_df")
  expect_equal(haven::zap_labels(haven::zap_formats(hr_117_local)), hr_117_online)

  # invalid filetype error
  tmp_pdf <- tempfile(fileext = ".pdf")
  expect_error(get_voteview_members(local_path = tmp_pdf), regexp = "Invalid `path` provided:")
})

test_that("local read filtering", {
  skip_if_offline()

  ## create filepath
  tmp_tsv <- tempfile(fileext = ".tsv")

  ## download and save from online
  members_70_73 <- get_voteview_members(chamber = "all", congress = 70:73)
  expect_s3_class(members_70_73, "tbl_df")
  expect_length(members_70_73, 22)
  expect_equal(nrow(members_70_73), 2226)
  readr::write_tsv(members_70_73, tmp_tsv)

  ## read whole file from local
  local_members_70_73 <- get_voteview_members(chamber = "all", congress = 70:73,
                                              local_path = tmp_tsv)
  expect_s3_class(local_members_70_73, "tbl_df")
  expect_equal(local_members_70_73, members_70_73)

  ## filter by chamber
  sen_70_73 <- get_voteview_members(chamber = "s", congress = 70:73,
                                    local_path = tmp_tsv)
  expect_s3_class(sen_70_73, "tbl_df")
  expect_equal(nrow(sen_70_73), 425)
  expect_equal(sen_70_73,
               dplyr::filter(members_70_73, chamber != "House") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  # shouldn't need `congress` arg for this file
  sen_70_73_v2 <- get_voteview_members(chamber = "s",
                                       local_path = tmp_tsv)
  expect_equal(sen_70_73_v2, sen_70_73)

  ## filter by congress
  members_73 <- get_voteview_members(congress = 73,
                                     local_path = tmp_tsv)
  expect_equal(nrow(members_73), 551)
  expect_equal(members_73, dplyr::filter(members_70_73, congress == 73))


  ## filter by both
  hr_70_71 <- get_voteview_members(chamber = "hr", congress = 70:71,
                                   local_path = tmp_tsv)
  expect_s3_class(hr_70_71, "tbl_df")
  expect_equal(nrow(hr_70_71), 903)
  expect_equal(hr_70_71,
               dplyr::filter(members_70_73,
                             chamber != "Senate",
                             congress %in% 70:71) |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  ## passing congress numbers not in the file
  # at least one congress is present: ok
  mems_73_not74 <- get_voteview_members(congress = 73:74, local_path = tmp_tsv)
  expect_s3_class(mems_73_not74, "tbl_df")
  expect_equal(nrow(mems_73_not74), 551)

  # none present: error
  expect_error(get_voteview_members(congress = c(10, 20, 5, 55, 100), local_path = tmp_tsv),
               "Congress numbers .+ were not found in data")
})
