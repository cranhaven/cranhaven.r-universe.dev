# random_string_if_pattern ####
test_that("combine_random_string_if_pattern works for various patterns", {
  set.seed(1)
  expect_equal(
    combine_random_string_if_pattern("random string", "random"),
    "GNZuCtwed3CAgNlUizNmvD"
  )
  expect_equal(
    combine_random_string_if_pattern("random string", "original"),
    "random string"
  )
})

# random_string_if_no_address ####
test_that("combine_random_string_if_no_address works for various patterns", {
  set.seed(1)
  expect_equal(
    combine_random_string_if_no_address(c("18, 20 London Road", "No trade address found")),
    c("18, 20 London Road", "GNZuCtwed3CAgNlUizNmvD")
  )
})

# no_trade_address_to_randon_string ####
test_that("combine_no_trade_address_to_random_string works in general", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    rank = c("135", "326"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    type = rep("OWN ACCOUNT", 2L),
    address.trade = c("18, 20, London Road.", "No trade address found")
  )
  set.seed(1)
  out <- tibble::tibble(
    page = rep("71", 2L),
    rank = c("135", "326"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    type = rep("OWN ACCOUNT", 2L),
    address.trade = c("18, 20, London Road.", "GNZuCtwed3CAgNlUizNmvD"),
  )
  expect_equal(combine_no_trade_address_to_random_string(directory), out)
})

# make_match_string ####
test_that("combine_make_match_string works for various entries", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    rank = c("135", "326"),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    type = rep("OWN ACCOUNT", 2L),
    address.trade = c("18, 20, London Road.", "No trade address found")
  )
  set.seed(1)
  out <- tibble::tibble(
    page = rep("71", 2L),
    rank = c("135", "326"),
    match.string = c(
      "Abbott William - 18, 20, London Road",
      "Abercromby Alexander - GNZuCtwed3CAgNlUizNmvD"
    ),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    type = rep("OWN ACCOUNT", 2L),
  )
  expect_equal(combine_make_match_string(directory), out)
})

# has_match_failed ####
test_that("combine_has_match_failed works in general", {
  numbers <- c("18, 20", NA)
  bodies <- c("London Road.", NA)
  expect_equal(combine_has_match_failed(numbers, bodies), c(FALSE, TRUE))
})

# label_if_match_failed ####
test_that("combine_label_if_match_failed works for both number and address body", {
  numbers <- c("18, 20", NA)
  bodies <- c("London Road.", NA)
  combine_label_if_match_failed("number", number = numbers, body = bodies)
  expect_equal(
    combine_label_if_match_failed("number", number = numbers, body = bodies),
    c("18, 20", "")
  )
  expect_equal(
    combine_label_if_match_failed("body", number = numbers, body = bodies),
    c("London Road.", "Failed to match with general directory")
  )
})

# get_address_house_type ####
test_that("combine_get_address_house_type works for body number and address house body", {

  expect_equal(combine_get_address_house_type("address.house.number"), "number")
  expect_equal(combine_get_address_house_type("address.house.body"), "body")
})

# label_failed_matches ####
test_that("combine_label_failed_matches works for various entries", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.house.number = c("136", NA),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", NA)
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.house.number = c("136", ""),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Failed to match with general directory")
  )
  expect_equal(combine_label_failed_matches(directory), out)
})


# combine_match_general_to_trades_plain ####
test_that("combine_match_general_to_trades_plain works in general", {
  trades_directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failed to match with general directory"
    )
  )
  expect_equal(
    combine_match_general_to_trades_plain(
      trades_directory, general_directory, verbose = FALSE, matches = FALSE,
      method = "osa", max_dist = 5
    ),
    out
  )
})

# combine_match_general_to_trades_progress ####
test_that("combine_match_general_to_trades_progress works in general", {
  trades_directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failed to match with general directory"
    )
  )
  expect_equal(
    combine_match_general_to_trades_progress(
      trades_directory, general_directory, verbose = FALSE, matches = FALSE,
      method = "osa", max_dist = 5
    ),
    out
  )
})

# match_general_to_trades ####

## plain ####
test_that("combine_match_general_to_trades works (plain)", {
  trades_directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failed to match with general directory"
    )
  )
  expect_equal(
    combine_match_general_to_trades(
      trades_directory, general_directory, progress = TRUE, verbose = FALSE,
      distance = FALSE, matches = FALSE, method = "osa", max_dist = 5
    ),
    out
  )
})

## distance ####
test_that("combine_match_general_to_trades works (distance)", {
  trades_directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failed to match with general directory"
    ),
    distance = c(0L, 4L, NA)
  )
  expect_equal(
    combine_match_general_to_trades(
      trades_directory, general_directory, progress = TRUE, verbose = FALSE,
      distance = TRUE, matches = FALSE, method = "osa", max_dist = 5
    ),
    out
  )
})

## matches ####
test_that("combine_match_general_to_trades works (matches)", {
  trades_directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failed to match with general directory"
    ),
    match = c(
      "Abbott William - 18, 20, London Road", "Abercromby Alexander - Dixon Place", NA
    )
  )
  expect_equal(
    combine_match_general_to_trades(
      trades_directory, general_directory, progress = TRUE, verbose = FALSE,
      distance = FALSE, matches = TRUE, method = "osa", max_dist = 5
    ),
    out
  )
})

## distance & matches ####
test_that("combine_match_general_to_trades works (distance & matches)", {
  trades_directory <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street")
  )
  general_directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", ""),
    address.house.number = c("136", "29"),
    address.trade.body = c("London Road", "Dixon Place"),
    address.house.body = c("Queen Square", "Anderston Quay")
  )
  out <- tibble::tibble(
    page = rep("71", 3L),
    rank = c("135", "326", "586"),
    surname = c("Abbott", "Abercromby", "Blair"),
    forename = c("William", "Alexander", "John Hugh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
    type = rep("OWN ACCOUNT", 3L),
    address.trade.number = c("18, 20", "12", "280"),
    address.trade.body = c("London Road", "Dixon Place", "High Street"),
    address.house.number = c("136", "29", ""),
    address.house.body = c(
      "Queen Square", "Anderston Quay", "Failed to match with general directory"
    ),
    distance = c(0L, 4L, NA),
    match = c(
      "Abbott William - 18, 20, London Road", "Abercromby Alexander - Dixon Place", NA
    )
  )
  expect_equal(
    combine_match_general_to_trades(
      trades_directory, general_directory, progress = TRUE, verbose = FALSE,
      distance = TRUE, matches = TRUE, method = "osa", max_dist = 5
    ),
    out
  )
})
