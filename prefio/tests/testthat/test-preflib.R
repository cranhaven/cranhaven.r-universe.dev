test_that("`preferences` ordinal type is detected appropriately", {
  toc <- read_preflib("../data/aspen00016-00000001.toc")$preferences
  toi <- read_preflib("../data/berkley00017-00000001.toi")$preferences
  soc <- read_preflib("../data/netflix00004-00000101.soc")$preferences
  soi <- read_preflib("../data/glasgow00008-00000003.soi")$preferences
  expect_equal("toc", attr(toc, "preftype"))
  expect_equal("toi", attr(toi, "preftype"))
  expect_equal("soc", attr(soc, "preftype"))
  expect_equal("soi", attr(soi, "preftype"))
})

test_that("`read_preflib` throws error when 'ALT NAME X' is missing", {
  expect_error(read_preflib("../data/corrupt/missing_alt.soc"))
})

test_that("`read_preflib` throws error when 'N ALTS' is missing", {
  expect_error(read_preflib("../data/corrupt/missing_nalts.soc"))
})

test_that("`read_preflib` throws error when 'N VTRS' is missing", {
  expect_error(read_preflib("../data/corrupt/missing_nvoters.soc"))
})

test_that("`read_preflib` raises warning when 'N VTRS' differs from data", {
  expect_warning(read_preflib("../data/corrupt/incorrect_n_voters.soc"))
})

test_that("`read_preflib` raises warning when 'N UNQ ORDS' differs from data", {
  expect_warning(
    read_preflib("../data/corrupt/incorrect_n_unique_orders.soc")
  )
})

test_that("`read_preflib` throws error when 'N UNQ ORDS' is not integral", {
  expect_error(read_preflib("../data/corrupt/non_integer_n_unq_ords.soc"))
})

toc <- read_preflib("../data/aspen00016-00000001.toc")
toi <- read_preflib("../data/berkley00017-00000001.toi")
soc <- read_preflib("../data/netflix00004-00000101.soc")
soi <- read_preflib("../data/glasgow00008-00000003.soi")
today <- format(Sys.time(), "%Y-%m-%d")

test_that("`write_preflib` writes to stdout for all test datasets", {
  expect_output(
    write_preflib(toc,
      title = "test toc",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test toc"
  )
  expect_output(
    write_preflib(toi,
      title = "test toi",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test toi"
  )
  expect_output(
    write_preflib(soc,
      title = "test soc",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test soc"
  )
  expect_output(
    write_preflib(soi,
      title = "test soi",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test soi"
  )
})

test_that("`write_preflib` produces a file which reads to identical object", {
  pfile <- testthat::capture_output(
    write_preflib(toc,
      title = "test toc tempfile",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    )
  )
  expect_true(all(read_preflib(textConnection(pfile)) == toc))
})

test_that("`write_preflib` and `read_preflib` work with single preferences", {
  onepref <- preferences(
    matrix(c(1, 2, NA, NA),
      nrow = 1,
      dimnames = list(NULL, LETTERS[1:4])
    ),
    format = "ranking",
    aggregate = TRUE
  )
  t <- tempfile()
  write_preflib(onepref,
    t,
    title = "test toc tempfile",
    modification_type = "imbued",
    modification_date = today,
    publication_date = today
  )
  onepref_from_t <- read_preflib(t)
  expect_true(all(onepref == onepref_from_t))
})
