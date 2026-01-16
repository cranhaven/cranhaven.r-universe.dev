# move_house_to_address ####
test_that("general_move_house_to_address works in general", {
  directory <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c(
      "Wine and spirit merchant; house", "Baker", "Victualer"
    ),
    addresses = c("1820 Mary hill", "", "280, High stret")
  )
  regex <- globals_regex_house_to_address
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c(
      "Wine and spirit merchant", "Baker", "Victualer"
    ),
    addresses = c("house, 1820 Mary hill", "", "280, High stret")
  )
  expect_equal(general_move_house_to_address(directory, regex), out)
})

# repatriate_occupation_from_address ####
test_that("general_repatriate_occupation_from_address works for various occupations", {
  directory <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("", "Wine and spirit merchant", ""),
    addresses = c("bkr; 1820, Mary hill", "", "Victualer; 280, High stret")
  )
  regex <- globals_regex_occupation_from_address
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("bkr", "Wine and spirit merchant", "Victualer"),
    addresses = c("1820, Mary hill", "", "280, High stret")
  )
  expect_equal(general_repatriate_occupation_from_address(directory, regex), out)
})

# split_trade_house_addresses ####
test_that("general_split_trade_house_addresses works for various addresses", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses = c(
      "18, 20 London Street; ho. 136 Queen Street.",
      "12 Dixon Street; res, 29 Anderston Quay."
    )
  )
  regex <- globals_regex_house_split_trade
  out <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses.trade = c("18, 20 London Street", "12 Dixon Street"),
    address.house = c("136 Queen Street.", "29 Anderston Quay.")
  )
  expect_equal(general_split_trade_house_addresses(directory, regex, verbose = FALSE), out)
})

# split_trade_house_addresses ####
test_that("general_split_trade_house_addresses works for various addresses", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses.trade = c(
      "18, 20 London Street, 136 Queen Street; 134, 136 South Portland Street",
      "12 Dixon Street, & 29 Anderston Quay; and 265 Argyle Street."
    )
  )
  regex_split <- globals_regex_split_trade_addresses
  regex_filter <- globals_regex_and_filter
  regex_match <- globals_regex_and_match
  out <- tibble::tibble(
    page = rep("71", 6L),
    surname = c(rep("Abbott", 3L), rep("Abercromby", 3L)),
    forename = c(rep("William", 3L), rep("Alexander", 3L)),
    occupation = c(rep("Wine and spirit merchant", 3L), rep("Baker", 3L)),
    address.trade = c(
      "18, 20 London Street.", "136 Queen Street.", "134, 136 South Portland Street.",
      "12 Dixon Street.", "29 Anderston Quay.", "265 Argyle Street."
    )
  )
  expect_equal(
    general_split_trade_addresses(
      directory, regex_split, regex_filter, regex_match,
      ignore_case_split = FALSE, ignore_case_filter = TRUE, ignore_case_match = FALSE
    ),
    out
  )
})

# split_address_numbers_bodies ####
test_that("general_split_address_numbers_bodies works for various addresses", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade = c("18, 20 London Street", "12 Dixon Street"),
    address.house = c("136 Queen Street", "265 Argyle Street")
  )
  regex_split_address_numbers <- globals_regex_split_address_numbers
  regex_split_address_body <- globals_regex_split_address_body
  regex_split_address_empty <- globals_regex_split_address_empty
  out <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"), address.house.number = c("136", "265"),
    address.trade.body = c("London Street.", "Dixon Street."),
    address.house.body = c("Queen Street.", "Argyle Street.")
  )
  expect_equal(
    general_split_address_numbers_bodies(
      directory, regex_split_address_numbers, regex_split_address_body,
      regex_split_address_empty, ignore_case_filter = TRUE, ignore_case_match = TRUE
    ),
    out
  )
})

# fix_structure ####
test_that("general_fix_structure works for various entries", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant; house", ""),
    addresses = c(
      "18, 20 London Street",
      "Baker; 12 Dixon Street, & 29 Anderston Quay; and 265 Argyle Street."
    )
  )
  out <- tibble::tibble(
    page = rep("71", 4L),
    surname = c("Abbott", rep("Abercromby", 3L)),
    forename = c("William", rep("Alexander", 3L)),
    occupation = c("Wine and spirit merchant", rep("Baker", 3L)),
    address.trade.number = c("", "12", "29", "265"),
    address.house.number = c("18, 20", "", "", ""),
    address.trade.body = c("", "Dixon Street.", "Anderston Quay.", "Argyle Street."),
    address.house.body = c("London Street.", "", "", "")
  )
  expect_equal(general_fix_structure(directory, FALSE), out)
})

# clean_entries ####
test_that("general_clean_entries works for various entries", {
  directory <- tibble::tibble(
    page = c("71", "71"),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", "Bkr"),
    address.trade.number = c("1S20", "I2"),
    address.house.number = c("13<J", "2G5"),
    address.trade.body = c("Londn rd.", "Dixen pl"),
    address.house.body = c("Queun sq", "Argul st")
  )
  out <- tibble::tibble(
    page = c("71", "71"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.trade.body = c("London Road.", "Dixon Place."),
    address.house.number = c("136", "265"),
    address.house.body = c("Queen Square.", "Argyle Street.")
  )
  expect_equal(general_clean_entries(directory, verbose = FALSE), out)
})

# general_clean_directory_progress ####
test_that("general_clean_directory_progress works in general", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", ""),
    addresses = c(
      "1S20 Londn rd; ho. 13<J Queun sq",
      "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
    )
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("Abbott", "Abercromby", "Abercromby"),
    forename = c("William", "Alexander", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker", "Baker"),
    address.trade.number = c("18, 20", "12", "29"),
    address.trade.body = c("London Road.", "Dixon Street.", "Anderston Quay."),
    address.house.number = c("136", "265", "265"),
    address.house.body = c("Queen Square.", "Argyle Street.", "Argyle Street.")
  )
  expect_equal(general_clean_directory_progress(directory, verbose = FALSE), out)
})

# general_clean_directory_plain ####
test_that("general_clean_directory_plain works in general", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", ""),
    addresses = c(
      "1S20 Londn rd; ho. 13<J Queun sq",
      "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
    )
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("Abbott", "Abercromby", "Abercromby"),
    forename = c("William", "Alexander", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker", "Baker"),
    address.trade.number = c("18, 20", "12", "29"),
    address.trade.body = c("London Road.", "Dixon Street.", "Anderston Quay."),
    address.house.number = c("136", "265", "265"),
    address.house.body = c("Queen Square.", "Argyle Street.", "Argyle Street.")
  )
  expect_equal(general_clean_directory_plain(directory, verbose = FALSE), out)
})

# clean_directory ####
test_that("general_clean_directory works in general", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit mercht — See Advertisement in Appendix.", ""),
    addresses = c(
      "1S20 Londn rd; ho. 13<J Queun sq",
      "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
    )
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    surname = c("Abbott", "Abercromby", "Abercromby"),
    forename = c("William", "Alexander", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker", "Baker"),
    address.trade.number = c("18, 20", "12", "29"),
    address.trade.body = c("London Road.", "Dixon Street.", "Anderston Quay."),
    address.house.number = c("136", "265", "265"),
    address.house.body = c("Queen Square.", "Argyle Street.", "Argyle Street.")
  )
  expect_equal(
    general_clean_directory(directory, progress = TRUE, verbose = FALSE), out
  )
})
