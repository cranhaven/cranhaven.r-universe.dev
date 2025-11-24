test_that("apply_klass returns correct names for numeric codes", {
  data(klassdata)
  kommune_names <- apply_klass(klassdata$kommune,
    classification = 131,
    date = "2015-01-01"
  )
  expect_equal(kommune_names[1], "Sandefjord")
})



test_that("apply_klass returns correct names for numeric codes", {
  data(klassdata)
  kommune_names <- apply_klass(klassdata$kommune,
    classification = 131,
    date = "2015-01-01"
  )
  expect_equal(kommune_names[1], "Sandefjord")
})


test_that("apply_klass returns correct names for character codes", {
  data(klassdata)
  sektor_names <- apply_klass(c("INNL", "UTL", "B_FIN"),
    classification = 39,
    date = "2019-01-01"
  )
  expect_equal(sektor_names[2], "Utlandet")
})


test_that("apply_klass returns correct language", {
  data(klassdata)
  occ <- apply_klass(
    klassdata$occupation,
    classification = 7,
    language = "en",
    date = "2015-01-01",
    format = F
  )
  expect_equal(occ[1], "Bartenders")
  occ <- apply_klass(
    klassdata$occupation,
    classification = 7,
    language = "nb",
    date = "2015-01-01",
    format = F
  )
  expect_equal(occ[4], "Elektronikkingeniører")
})


test_that("apply_klass returns correctly for character codes", {
  sektor <- c("INNL", "UTL")
  sektor_apply <- apply_klass(sektor, 39, date = "2020-01-01")
  expect_equal(sektor_apply[2], "Utlandet")
})


test_that("Check levelCheck for numerical codes", {
  occ <- get_klass(7, date = "2020-01-01")
  expect_equal(levelCheck("031", occ), 3)
  expect_equal(levelCheck(c("0210", "021"), occ), 3)
  sn <- get_klass(6, date = "2020-01-01")
  expect_equal(levelCheck("03.1", sn), 3)
  expect_equal(levelCheck("A", sn), 1) # Character code but added in here
})


test_that("Check levelCheck for character codes", {
  sek <- get_klass(39, date = "2020-01-01")
  expect_equal(levelCheck("INNL", sek), 1)
  expect_equal(levelCheck(c("INNL", "B_FIN", "B_FIN", NA), sek), 2)
})


test_that("apply_klass can return a variant classification", {
  dat <- c("000", "101", "102", "103")
  dat <- c("01.21", "01.46", "10.61")

  dat_new <- klassR::apply_klass(dat,
    classification = 6,
    variant = 1616,
    date = "2021-01-02"
  )
  expect_equal(dat_new[2], "Svinehold")

  dat_new <- klassR::apply_klass(
    dat,
    classification = 6,
    variant = 1616,
    output_level = 1,
    date = "2021-01-02"
  )
  expect_equal(dat_new[3], "Industri")
})


test_that(
  "apply_klass works for variant classification with Norwegian characters in the variant name",
  {
    dat <- c("05", "01")
    new <- apply_klass(
      dat,
      classification = 6,
      variant = 1616,
      output_level = 1,
      output = "name",
      date = "2020-01-01"
    )
    expect_equal(new[1], "Bergverksdrift og utvinning")
  }
)


test_that("An error is correctly returned in the case of a null vector", {
  expect_error(apply_klass(NULL, 131), "The input vector is empty")
})


test_that("apply_klass works for classifications with varying number of digits", {
  dat <- c("56", "580")
  new <- apply_klass(dat, classification = 270, date = "2024-01-01")
  expect_false(new[1] == new[2])
  expect_false(all(is.na(new)))
})


test_that("Nace classification with missing formatting", {
  # simple example with all missing formatting
  dat <- c("01460", "45112", "45111", "45310")
  expect_warning(
    new <- apply_klass(dat, classification = 6, date = "2024-01-01"),
    "Number missing .: 4"
  )
  expect_equal(new[1], "Svinehold")

  # Check mixture of formatting
  dat <- c("45112", "45.111")
  expect_warning(
    new <- apply_klass(dat, classification = 6, date = "2024-01-01"),
    "Number missing .: 1"
  )
  expect_equal(
    new[1],
    "Detaljhandel med biler og lette motorvogner, unntatt motorsykler"
  )

  # checking NAs
  dat <- c("45.112", "45.111", NA)
  expect_warning(
    new <- apply_klass(dat, classification = 6, date = "2024-01-01"),
    "Number of NA: 1"
  )
  expect_equal(
    new[1],
    "Detaljhandel med biler og lette motorvogner, unntatt motorsykler"
  )
  expect_true(is.na(new[3]))
})


test_that("Municipality classification with missing formatting", {
  # example with all missing leading 0
  dat <- c("301", "301")
  expect_warning(
    new <- apply_klass(dat, classification = 131, date = "2024-01-01"),
    "Number missing leading 0: 2"
  )
  expect_equal(new[1], "Oslo")

  # simple example with all missing leading 0
  dat <- c("301", "301")
  expect_warning(
    new <- apply_klass(dat, classification = 131, date = "2024-01-01"),
    "Number missing leading 0: 2"
  )
  expect_equal(new[1], "Oslo")


  # Check mixture of formatting
  dat <- c("45112", "45.111")
  expect_warning(
    new <- apply_klass(dat, classification = 6, date = "2024-01-01"),
    "Number missing .: 1"
  )
  expect_equal(
    new[1],
    "Detaljhandel med biler og lette motorvogner, unntatt motorsykler"
  )
})


test_that("apply_klass works for classifications with varying digits and letters", {
  dat <- c("56", "580", "KG1")
  new <- apply_klass(dat, classification = 270, date = "2024-01-01")
  expect_false(all(is.na(new)))

  dat <- c("01", "03b")
  new2 <- apply_klass(dat, classification = 207, date = "2024-01-01")
  expect_false(all(is.na(new2)))
})


test_that("An error is correctly returned in the case of a null vector", {
  expect_error(apply_klass(NULL, 131), "The input vector is empty")
})
