# clean_entries ####
test_that("trades_clean_entries works for various entries", {
  directory <- data.frame(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"),
    forename = c("Wm.", "Alex", "Jon Hug"),
    occupation = c(
      "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr",
      "Victualer"
    ),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("1S20", "I2", "2S0"),
    address.trade.body = c("Londn rd.", "Dixen pl", "High St."),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road.", "Dixon Place.", "High Street."),
    stringsAsFactors = FALSE
  )
  expect_equal(trades_clean_entries(directory, verbose = FALSE), out)
})


# clean_directory ####
test_that("trades_clean_directory works in general", {
  directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"),
    forename = c("Wm.", "Alex", "Jon Hug"),
    occupation = c(
      "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr",
      "Victualer"
    ),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("1S20", "I2", "2S0"),
    address.trade.body = c("Londn rd.", "Dixen pl", "High St.")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road.", "Dixon Place.", "High Street."),
  )
  expect_equal(trades_clean_directory(directory, verbose = FALSE), out)
})

# trades_clean_directory_plain ####
test_that("trades_clean_directory_plain works in general", {
  directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"),
    forename = c("Wm.", "Alex", "Jon Hug"),
    occupation = c(
      "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr",
      "Victualer"
    ),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("1S20", "I2", "2S0"),
    address.trade.body = c("Londn rd.", "Dixen pl", "High St.")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road.", "Dixon Place.", "High Street."),
  )
  expect_equal(trades_clean_directory_plain(directory, verbose = FALSE), out)
})

# trades_clean_directory_progress ####
test_that("trades_clean_directory_progress works in general", {
  directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"),
    forename = c("Wm.", "Alex", "Jon Hug"),
    occupation = c(
      "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr",
      "Victualer"
    ),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("1S20", "I2", "2S0"),
    address.trade.body = c("Londn rd.", "Dixen pl", "High St.")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road.", "Dixon Place.", "High Street."),
  )
  expect_equal(trades_clean_directory_progress(directory, verbose = FALSE), out)
})

# clean_directory ####
test_that("trades_clean_directory works in general", {
  directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"),
    forename = c("Wm.", "Alex", "Jon Hug"),
    occupation = c(
      "Wine and spirit mercht — See Advertisement in Appendix.", "Bkr",
      "Victualer"
    ),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("1S20", "I2", "2S0"),
    address.trade.body = c("Londn rd.", "Dixen pl", "High St.")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road.", "Dixon Place.", "High Street."),
  )
  expect_equal(
    trades_clean_directory(directory, progress = TRUE, verbose = FALSE), out
  )
})
