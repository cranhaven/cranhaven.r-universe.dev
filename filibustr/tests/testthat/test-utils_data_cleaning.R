test_that("filter_chamber()", {
  skip_if_offline()

  # testing with `get_voteview_members()`
  members_94 <- get_voteview_members(congress = 94)
  expect_s3_class(members_94, "tbl_df")
  expect_equal(nrow(members_94), 543)

  # no filter
  expect_equal(filter_chamber(members_94, chamber = "all"),
               members_94)
  expect_equal(filter_chamber(members_94, chamber = "congress"),
               members_94)
  expect_equal(filter_chamber(members_94, chamber = "HS"),
               members_94)

  # filter to Senate
  sen_94 <- filter_chamber(members_94, chamber = "s")
  expect_s3_class(sen_94, "tbl_df")
  expect_equal(nrow(sen_94), 102)
  expect_equal(sen_94,
               dplyr::filter(members_94, chamber != "House") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  expect_equal(filter_chamber(members_94, "senate"), sen_94)
  expect_equal(filter_chamber(members_94, "sen"), sen_94)

  # filter to House
  hr_94 <- filter_chamber(members_94, chamber = "hr")
  expect_s3_class(hr_94, "tbl_df")
  expect_equal(nrow(hr_94), 442)
  expect_equal(hr_94,
               dplyr::filter(members_94, chamber != "Senate") |>
                 dplyr::mutate(chamber = droplevels(chamber)))

  expect_equal(filter_chamber(members_94, "h"), hr_94)
  expect_equal(filter_chamber(members_94, "house"), hr_94)
})

test_that("filter_congress()", {
  skip_if_offline()

  # base sheet: all members
  all_members <- get_voteview_members()
  expect_s3_class(all_members, "tbl_df")
  expect_gt(nrow(all_members), 50400)

  # single congress
  members_1 <- filter_congress(all_members, 1)
  expect_s3_class(members_1, "tbl_df")
  expect_equal(nrow(members_1), 90)
  expect_equal(levels(members_1$chamber), c("President", "House", "Senate"))
  expect_equal(unique(members_1$congress), 1)

  members_117 <- filter_congress(all_members, 117)
  expect_s3_class(members_117, "tbl_df")
  expect_equal(nrow(members_117), 559)
  expect_equal(levels(members_117$chamber), c("President", "House", "Senate"))
  expect_equal(unique(members_117$congress), 117)

  # multiple congresses
  # continuous
  members_114_117 <- filter_congress(all_members, 114:117)
  expect_s3_class(members_114_117, "tbl_df")
  expect_equal(nrow(members_114_117), 2211)
  expect_equal(unique(members_114_117$congress), 114:117)
  expect_equal(dplyr::filter(members_114_117, congress == 117),
               members_117)

  # discontinuous
  members_various <- filter_congress(all_members, c(5, 1, 100, 117, 95))
  expect_s3_class(members_various, "tbl_df")
  expect_equal(nrow(members_various), 1901)
  # row order not impacted by order of Congress numbers in `congress` arg
  expect_equal(unique(members_various$congress), c(1, 5, 95, 100, 117))
  expect_equal(dplyr::filter(members_various, congress == 1),
               members_1)
  expect_equal(dplyr::filter(members_various, congress == 117),
               members_117)

  # all congresses
  expect_equal(filter_congress(all_members, NULL), all_members)
  expect_equal(filter_congress(all_members, 1:current_congress()), all_members)
})

test_that("filter_congress() error: invalid Congress number", {
  skip_if_offline()

  all_house <- get_voteview_members(chamber = "hr")

  expect_error(filter_congress(all_house, -1), "Invalid `congress` argument")
  expect_error(filter_congress(all_house, 0), "Invalid `congress` argument")
  expect_error(filter_congress(all_house, current_congress() + 1),
               "Invalid `congress` argument")
  # error if any congress number is invalid
  expect_error(filter_congress(all_house, c(1, 5, 500)), "Invalid `congress` argument")

  # other types
  expect_error(filter_congress(all_house, "-1"), "Invalid `congress` argument")
  expect_error(filter_congress(all_house, "word"), "Invalid `congress` argument")
  expect_error(filter_congress(all_house, FALSE), "Invalid `congress` argument")
})

test_that("filter_congress() error: Congress numbers not found", {
  skip_if_offline()

  all_sens <- get_voteview_members("s")
  expect_s3_class(all_sens, "tbl_df")
  expect_gt(nrow(all_sens), 10000)
  if (is_odd_year_january()) {
    expect_true(all.equal(unique(all_sens$congress), 1:current_congress()) ||
                  all.equal(unique(all_sens$congress), 1:(current_congress() - 1)))
  } else {
    expect_equal(unique(all_sens$congress), 1:current_congress())
  }

  # filter that dataset
  sens_100s <- filter_congress(all_sens, 100:109)
  expect_s3_class(sens_100s, "tbl_df")
  expect_equal(nrow(sens_100s), 1025)
  expect_equal(unique(sens_100s$congress), 100:109)

  # can filter with number in dataset
  expect_s3_class(filter_congress(sens_100s, 108), "tbl_df")

  # valid congress number that isn't in the dataset
  expect_s3_class(filter_congress(all_sens, 110), "tbl_df")
  expect_error(filter_congress(sens_100s, 110), "Congress number .+ was not found")

  # pluralization
  expect_error(filter_congress(sens_100s, 110:112), "Congress numbers .+ were not found")

  # ok if only some congress numbers are missing
  sens_108_not112 <- filter_congress(sens_100s, 108:112)
  expect_s3_class(sens_108_not112, "tbl_df")
  expect_equal(nrow(sens_108_not112), 203)
})
