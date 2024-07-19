test_that("documentation for `local` arg", {
  expect_equal(
    doc_arg_local("Voteview"),
    paste("Whether to read the data from a local file, as opposed to the Voteview website.",
          "Default is `TRUE`.",
          "If the local file does not exist, will fall back to reading from online.")
  )
  expect_equal(
    doc_arg_local("Harvard Dataverse"),
    paste("Whether to read the data from a local file, as opposed to the Harvard Dataverse website.",
          "Default is `TRUE`.",
          "If the local file does not exist, will fall back to reading from online.")
  )
  expect_equal(
    doc_arg_local("Center for Effective Lawmaking"),
    paste("Whether to read the data from a local file, as opposed to the Center for Effective Lawmaking website.",
          "Default is `TRUE`.",
          "If the local file does not exist, will fall back to reading from online.")
  )
})

test_that("get online data: Voteview members", {
  vv_resp_members_s117 <- get_online_data(
    "https://voteview.com/static/data/out/members/S117_members.csv", "Voteview")
  expect_type(vv_resp_members_s117, "character")
  # check that CSV format works
  members_s117_df <- readr::read_csv(vv_resp_members_s117, show_col_types = FALSE)
  expect_s3_class(members_s117_df, "tbl_df")
  expect_length(members_s117_df, 22)
  expect_equal(nrow(members_s117_df), 104)
  expect_equal(unique(members_s117_df$chamber), c("President", "Senate"))
  expect_equal(unique(members_s117_df$congress), 117)
})

test_that("get online data: Voteview parties", {
  vv_resp_parties <- get_online_data(
    "https://voteview.com/static/data/out/parties/HSall_parties.csv", "Voteview")
  expect_type(vv_resp_parties, "character")
  # check that CSV format works
  parties_df <- readr::read_csv(vv_resp_parties, show_col_types = FALSE)
  expect_s3_class(parties_df, "tbl_df")
  expect_length(parties_df, 9)
  expect_equal(unique(parties_df$chamber), c("President", "House", "Senate"))
  expect_equal(unique(parties_df$congress), 1:118)
})
