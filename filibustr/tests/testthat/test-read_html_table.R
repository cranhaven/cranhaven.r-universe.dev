test_that("read_html_table()", {
  skip_if_offline()

  # The table used in `get_senate_cloture_votes()`
  # NOTE: `get_senate_cloture_votes()` performs some cleaning on this table
  cloture_votes <- read_html_table(
    url = "https://www.senate.gov/legislative/cloture/clotureCounts.htm",
    css = ".cloturecount"
  )

  expect_s3_class(cloture_votes, "tbl_df")
  expect_length(cloture_votes, 5)
  # this table will add one row each Congress
  expect_equal(nrow(cloture_votes), 55 + (current_congress() - 118))
  expect_equal(colnames(cloture_votes),
               c("Congress", "Years", "Motions Filed", "Votes on Cloture", "Cloture Invoked"))


  # A table from Baseball Reference: Steve Garvey stats
  br_table <- read_html_table(
    url = "https://www.baseball-reference.com/players/g/garvest01.shtml",
    css = "#players_standard_batting"
  )

  expect_s3_class(br_table, "tbl_df")
  expect_length(br_table, 33)
  expect_equal(nrow(br_table), 24)
})
