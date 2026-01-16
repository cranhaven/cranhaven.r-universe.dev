# make_path ####
test_that("utils_make_path works for various number of arguments", {
  expect_equal(
    utils_make_path("home", "projects", "glasgow-entrepreneurs.csv"),
    "home/projects/glasgow-entrepreneurs.csv"
  )
  expect_equal(
    utils_make_path("home", "code", "projects", "glasgow-entrepreneurs.csv"),
    "home/code/projects/glasgow-entrepreneurs.csv"
  )
})

## make_file ####
test_that("utils_make_file works for various number of arguments", {
  expect_equal(
    utils_make_file("glasgow", "entrepreneurs", extension = "csv"),
    "glasgow-entrepreneurs.csv"
  )
  expect_equal(
    utils_make_file("glasgow", "scotland", "entrepreneurs", extension = "csv"),
    "glasgow-scotland-entrepreneurs.csv"
  )
})

## IO_path ####
test_that("utils_IO_path works for various extension types", {
  expect_equal(
    utils_IO_path("home/projects", "glasgow-entrepreneurs", extension = "csv"),
    "home/projects/glasgow-entrepreneurs.csv"
  )
  expect_equal(
    utils_IO_path("home/projects", "glasgow-entrepreneurs", extension = "txt"),
    "home/projects/glasgow-entrepreneurs.txt"
  )
})

## IO_path ####
test_that("utils_IO_path works for various extension types", {
  expect_equal(
    utils_IO_path("home/projects", "glasgow-entrepreneurs", extension = "csv"),
    "home/projects/glasgow-entrepreneurs.csv"
  )
  expect_equal(
    utils_IO_path("home/projects", "glasgow-entrepreneurs", extension = "txt"),
    "home/projects/glasgow-entrepreneurs.txt"
  )
})

## split_and_name ####
test_that("utils_split_and_name works for various number of columns", {
  out <- tibble::tibble(location = "glasgow", occupation = "entrepreneurs")
  expect_equal(
    utils_split_and_name(
      "glasgow-entrepreneurs", "-", 2, c("location", "occupation")
    ),
    out
  )
  out <- tibble::tibble(
    country = "scotland", city = "glasgow", occupation = "entrepreneurs"
  )
  expect_equal(
    utils_split_and_name(
      "scotland-glasgow-entrepreneurs", "-", 3, c("country", "city", "occupation")
    ),
    out
  )
})

## clear_content ####
test_that("utils_clear_content works for various regexes", {
  expect_equal(
    utils_clear_content("glasgow-entrepreneurs", "^.+-", TRUE), "entrepreneurs"
  )
  expect_equal(
    utils_clear_content("glasgow-entrepreneurs", "-.+$", TRUE), "glasgow"
  )
})

## execute ####
test_that("utils_execute works for various functions and arguments", {
  expect_equal(
    utils_execute(
      FALSE, utils_make_path, "home", "projects", "glasgow-entrepreneurs.csv"
    ),
    "home/projects/glasgow-entrepreneurs.csv"
  )
  expect_equal(
    utils_execute(
      FALSE, utils_make_file, "glasgow", "entrepreneurs", extension = "csv"
    ),
    "glasgow-entrepreneurs.csv"
  )
})

## mutate_across ####
test_that("utils_mutate_across works for various functions and arguments", {
  df <- tibble::tibble(location = "glasgow", occupation = "wine merchant")
  out <- tibble::tibble(location = "glasgow!", occupation = "wine merchant!")
  expect_equal(
    utils_mutate_across(df, dplyr::matches("location|occupation"), paste0, "!"),
    out
  )
  df <- tibble::tibble(location = "glasgow", occupation = "  wine   merchant")
  out <- tibble::tibble(location = "glasgow", occupation = "wine merchant")
  expect_equal(utils_mutate_across(df, "occupation", stringr::str_squish), out)
})

## paste_if_found ####
test_that("utils_paste_if_found works for various patterns and arguments", {
  expect_equal(
    utils_paste_if_found(
      "^glasgow", c("glasgow entrepreneurs", "aberdeen entrepreneurs"),
      "pattern not found", TRUE, "pattern", "found"
    ), c("pattern found", "pattern not found")
  )
  expect_equal(
    utils_paste_if_found(
      "entrepreneurs$", c("glasgow entrepreneurs", "aberdeen entrepreneurs"),
      "pattern not found", TRUE, "pattern", "found"
    ), c("pattern found", "pattern found")
  )
})

## gsub_if_found ####
test_that("utils_paste_if_found works for various patterns and arguments", {
  expect_equal(
    utils_gsub_if_found(
      "^glasgow", c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"),
      "(?<=-).+$", "merchant", "edinburgh-entrepreneurs",
      "pattern not found", TRUE, TRUE
    ), c("edinburgh-merchant", "pattern not found")
  )
  expect_equal(
    utils_gsub_if_found(
      "^glasgow", c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"),
      "^.+(?=-)", "merchant", "edinburgh-entrepreneurs",
      "pattern not found", TRUE, TRUE
    ), c("merchant-entrepreneurs", "pattern not found")
  )
})

## gsub_if_found ####
test_that("utils_gsub_if_found works for various patterns and arguments", {
  expect_equal(
    utils_gsub_if_found(
      "^glasgow", c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"),
      "(?<=-).+$", "merchant", "edinburgh-entrepreneurs",
      "pattern not found", TRUE, TRUE
    ), c("edinburgh-merchant", "pattern not found")
  )
  expect_equal(
    utils_gsub_if_found(
      "^glasgow", c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"),
      "^.+(?=-)", "merchant", "edinburgh-entrepreneurs",
      "pattern not found", TRUE, TRUE
    ), c("merchant-entrepreneurs", "pattern not found")
  )
})

## regmatches_if_found ####
test_that("utils_regmatches_if_found works for various patterns and arguments", {
  expect_equal(
    utils_regmatches_if_found(
      c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"), "^glasgow",
      "edinburgh-entrepreneurs", "^.+(?=-)", "merchant", TRUE, TRUE, FALSE
    ), c("edinburgh", "merchant")
  )
  expect_equal(
    utils_regmatches_if_found(
      c("glasgow-entrepreneurs", "aberdeen-bakers"), "bakers$",
      "edinburgh-entrepreneurs", "(?<=-).+$", "merchants", TRUE, TRUE, FALSE
    ), c("merchants", "entrepreneurs")
  )
})

## regmatches_if_not_empty ####
test_that("utils_regmatches_if_not_empty works for various patterns and arguments", {
  expect_equal(
    utils_regmatches_if_not_empty(
      c("glasgow-entrepreneurs", "", "aberdeen-entrepreneurs"),
      "edinburgh-entrepreneurs" , "^edinburgh", TRUE
    ), list("edinburgh", "", "edinburgh")
  )
  expect_equal(
    utils_regmatches_if_not_empty(
      c("glasgow-entrepreneurs", "", "aberdeen-entrepreneurs"),
      "edinburgh-entrepreneurs" , "(?<=-).+$", TRUE
    ), list("entrepreneurs", "", "entrepreneurs")
  )
})


## format_directory_raw ####
test_that("utils_format_directory_raw works for various directory names and entries", {
  directory <- tibble::tibble(
    page = rep(71L, 2L),
    surname = c("ABBOTT     ", " ABERCROMBY"), forename = c("Wm.", "Alex"),
    occupation = c("wine and spirit merchant", "baker"),
    addresses = c(
      "18, 20 London    st.; house, Mary hill.",
      "12 Dixon st.; residence,    Craigrownie, Cove.   "
    )
  )
  out <- tibble::tibble(
    directory = c("1861-1862", "1861-1862"),
    page = rep(71L, 2L),
    surname = c("ABBOTT", "ABERCROMBY"), forename = c("Wm.", "Alex"),
    occupation = c("wine and spirit merchant", "baker"),
    addresses = c(
      "18, 20 London st.; house, Mary hill.",
      "12 Dixon st.; residence, Craigrownie, Cove."
    )
  )
  expect_equal(utils_format_directory_raw(directory, "1861-1862"), out)

  directory <- tibble::tibble(
    page = rep(71L, 2L),
    surname = c("   ABBOTT ", " ABERCROMBY"), forename = c("Wm.", "Alex"),
    occupation = c("wine and spirit merchant", "baker"),
    addresses = c(
      "18, 20 London    st.; house, Mary hill.",
      "12 Dixon st.; residence,    Craigrownie, Cove.   "
    )
  )
  out <- tibble::tibble(
    directory = c("1863-1864", "1863-1864"),
    page = rep(71L, 2L),
    surname = c("ABBOTT", "ABERCROMBY"), forename = c("Wm.", "Alex"),
    occupation = c("wine and spirit merchant", "baker"),
    addresses = c(
      "18, 20 London st.; house, Mary hill.",
      "12 Dixon st.; residence, Craigrownie, Cove."
    )
  )
  expect_equal(utils_format_directory_raw(directory, "1863-1864"), out)
})

## clear_irrelevants ####
test_that("utils_clear_irrelevants works for various entries", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant — See Advertisement in Appendix.", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.house.number = c("136", "265"),
    address.trade.body = c("London Street.", "Dixon Street."),
    address.house.body = c("Queen Street.", "Argyle Street")
  )
  regex <- globals_regex_irrelevants
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c("18, 20", "12"),
    address.house.number = c("136", "265"),
    address.trade.body = c("London Street.", "Dixon Street."),
    address.house.body = c("Queen Street.", "Argyle Street")
  )
  expect_equal(utils_clear_irrelevants(directory, regex, ignore_case = TRUE), out)
})

## clean_occupations ####
test_that("utils_clean_occupations works for various occupations", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABBOTT", "ABERCROMBY"), forename = c("Wm.", "Alex"),
    occupation = c("wine and spirit mercht", "bkr"),
    addresses = c(
      "18, 20 Londonst.; house, Mary hill.",
      "12 Dixon st.; residence, Craigrownie, Cove."
    )
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABBOTT", "ABERCROMBY"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses = c(
      "18, 20 Londonst.; house, Mary hill.",
      "12 Dixon st.; residence, Craigrownie, Cove."
    )
  )
  expect_equal(utils_clean_occupations(directory), out)
})

## clean_names ####
test_that("utils_clean_names works for various names", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses = c(
      "18, 20 London st.; house, Mary hill.",
      "12 Dixon st.; residence, Craigrownie, Cove."
    )
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses = c(
      "18, 20 London st.; house, Mary hill.",
      "12 Dixon st.; residence, Craigrownie, Cove."
    )
  )
  expect_equal(utils_clean_names(directory), out)
})

## clean_address_ends ####
test_that("utils_clean_address_ends works for various addresses", {
  expect_equal(
    utils_clean_address_ends(
      c(
        " -; 18, 20 London st.; house, Mary hill.;,",
        "&,,12 Dixon st.; residence, Craigrownie, Cove.$"
      )
    ),
    c("18, 20 London st.; house, Mary hill.", "12 Dixon st.; residence, Craigrownie, Cove.")
  )
})

## clean_address_body ####
test_that("utils_clean_address_body works for various address bodies", {
  expect_equal(
    utils_clean_address_body(
      c("London st.", "Mary hill.;,", "&;Dixon st.", "Craigrownie, Cove.$")
    ),
    c("London Street.", "Maryhill.", "Dixon Street.", "Craigrownie Cove.")
  )
})

## clean_address ####
test_that("utils_clean_address works for various address components", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.number = c("-;1820", ",,12"),
    address.body = c(
      "London st. ; house, Mary hill.*",
      "&;Dixon st.; residence, Craigrownie, Cove.$"
    )
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.number = c("18, 20", "12"),
    address.body = c(
      "London st. ; house, Mary hill.*",
      "&;Dixon st.; residence, Craigrownie, Cove.$"
    )
  )
  expect_equal(utils_clean_address(directory, "number"), out)

  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.number = c("-;1820", ",,12"),
    address.body = c(
      "London Street House, Maryhill.",
      "Dixon Street, residence Craigrownie Cove."
    )
  )
  expect_equal(utils_clean_address(directory, "body"), out)

  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.number = c("18, 20", "12"),
    address.body = c(
      "London st. ; house, Mary hill.*",
      "&;Dixon st.; residence, Craigrownie, Cove.$"
    )
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.number = c("18, 20", "12"),
    address.body = c(
      "London st. ; house, Mary hill.",
      "Dixon st.; residence, Craigrownie, Cove."
    )
  )
  expect_equal(utils_clean_address(directory, "ends"), out)
})


## remove_address_prefix ####
test_that("utils_remove_address_prefix works for various pre-fixes", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses = c(
      "depot -; 1820 London    st. ; house, Mary hill.*",
      "workshop,,12 &;Dixon st.; residence,    Craigrownie, Cove.$   "
    )
  )
  regex <- globals_regex_address_prefix
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    addresses = c(
      " -; 1820 London    st. ; house, Mary hill.*",
      ",,12 &;Dixon st.; residence,    Craigrownie, Cove.$   "
    )
  )
  expect_equal(utils_remove_address_prefix(directory, regex, TRUE), out)
})


## label_missing_addresses ####
test_that("utils_label_missing_addresses works in general", {
  directory <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c(" -; 1820", ""),
    address.trade.body = c(
      "London st. ; house, Mary hill.*",
      ""
    )
  )
  out <- tibble::tibble(
    page = rep("71", 2L),
    surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
    occupation = c("Wine and spirit merchant", "Baker"),
    address.trade.number = c(" -; 1820", ""),
    address.trade.body = c(
      "London st. ; house, Mary hill.*",
      "No trade address found"
    )
  )
  expect_equal(utils_label_missing_addresses(directory), out)
})

## clean_addresses ####
test_that("utils_clean_addresses works for various addresses", {
  directory <- tibble::tibble(
    page = c("71", "71", "71"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
    address.trade.number = c(" -; 1820", "", "280"),
    address.trade.body = c("London st. ; house, Mary hill.*", "", "High stret")
  )
  out <- tibble::tibble(
    page = c("71", "71", "71"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
    address.trade.number = c("18, 20", "", "280"),
    address.trade.body = c(
      "London Street House, Maryhill.", "No trade address found", "High Street."
    )
  )
  expect_equal(utils_clean_addresses(directory), out)
})

## clean_ends ####
test_that("utils_clean_addresses works for various columns", {
  directory <- tibble::tibble(
    page = c("71", "71", "71"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
    address.trade.number = c(" -; 1820", "", "280"),
    address.trade.body = c("London st. ; house, Mary hill.*", "", "High streit")
  )
  out <- tibble::tibble(
    page = c("71", "71", "71"),
    surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
    occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
    address.trade.number = c("1820", "", "280"),
    address.trade.body = c("London st. ; house, Mary hill", "", "High streit")
  )
  expect_equal(
    utils_clean_ends(directory, address.trade.number, address.trade.body), out
  )
})
