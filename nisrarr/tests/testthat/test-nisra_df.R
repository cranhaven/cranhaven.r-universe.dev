test_that("nisra_df works", {
  expect_s3_class(
    nisra_df(x = 1:2),
    "nisra_df"
  )

  expect_snapshot(
    nisra_df(x = 1:2, meta = list(label = "test"))
  )
})

test_that("nisra_meta works", {
  nm <- new_nisra_meta(list(
    label = "test_meta",
    subect = list(value = "test_subject_value"),
    official = TRUE,
    experimental = TRUE,
    note = "this is a note which is more than one hundred characters long. this note will be truncated to create a shorter note",
    updated = "2050-05-30",
    contact = list(name = "test", email = "test_email", phone = "0800TEST"),
    copyright = list(name = "test", href = "copyright")
  ))

  expect_s3_class(nm, "nisra_meta")
  expect_snapshot(print(nm))
})

test_that("official_stat_type works", {
  expect_equal(
    official_stat_type(
      c(TRUE, FALSE, TRUE, FALSE),
      c(TRUE, TRUE, FALSE, FALSE)
    ),
    c(
      "Official statistics in development",
      "Not official statistics",
      "Official statistics",
      "Not official statistics"
    )
  )
})
