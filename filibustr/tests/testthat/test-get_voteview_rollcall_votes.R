test_that("download from Voteview", {
  online_rollcalls <- get_voteview_rollcall_votes(local = FALSE)
  expect_s3_class(online_rollcalls, "tbl_df")
  expect_length(online_rollcalls, 18)
  expect_equal(levels(online_rollcalls$chamber), c("House", "Senate"))
  expect_equal(unique(online_rollcalls$congress), 1:current_congress())
})

test_that("filter rollcalls by chamber", {
  s_votes <- get_voteview_rollcall_votes(chamber = "s")
  expect_s3_class(s_votes, "tbl_df")
  expect_length(s_votes, 18)
  expect_equal(levels(s_votes$chamber), "Senate")
  expect_equal(unique(s_votes$congress), 1:current_congress())

  hr_votes <- get_voteview_rollcall_votes(chamber = "hr")
  expect_s3_class(hr_votes, "tbl_df")
  expect_length(hr_votes, 18)
  expect_equal(levels(hr_votes$chamber), "House")
  expect_equal(unique(hr_votes$congress), 1:current_congress())

  # House has more recorded votes
  expect_lt(nrow(s_votes), nrow(hr_votes))
})

test_that("filter rollcalls by congress", {
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
