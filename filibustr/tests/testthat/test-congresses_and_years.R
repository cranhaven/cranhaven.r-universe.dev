test_that("years to congress numbers", {
  ## proper usage
  # numbers
  expect_equal(congress_in_year(1789), 1)
  expect_equal(congress_in_year(2000), 106)
  expect_equal(congress_in_year(2023), 118)
  expect_equal(congress_in_year(2100), 156)
  expect_equal(congress_in_year(3000), 606)

  # Date objects
  expect_equal(congress_in_year(as.Date("1789-01-01")), 1)
  expect_equal(congress_in_year(as.Date("1800-07-01")), 6)
  expect_equal(congress_in_year(as.Date("1800-07-01")), 6)
  expect_equal(congress_in_year(as.Date("2021-01-06")), 117)

  ## error handling
  expect_error(congress_in_year("word"),
               regexp = "Must provide the year as a number or Date object.",
               fixed = TRUE)
  expect_error(congress_in_year("1901"),
               regexp = "Must provide the year as a number or Date object.",
               fixed = TRUE)
  expect_error(congress_in_year(1788),
               regexp = paste("The provided year (1788) is too early.",
                              "The first Congress started in 1789."),
               fixed = TRUE)
  expect_error(congress_in_year(110),
               regexp = paste("The provided year (110) is too early.",
                              "The first Congress started in 1789."),
               fixed = TRUE)
  expect_error(congress_in_year(as.Date("1492-09-01")),
               regexp = paste("The provided year (1492) is too early.",
                              "The first Congress started in 1789."),
               fixed = TRUE)
})

test_that("congress numbers to years", {
  # basics
  expect_equal(year_of_congress(1), 1789)
  expect_equal(year_of_congress(57), 1901)
  expect_equal(year_of_congress(118), 2023)

  # error handling
  expect_error(year_of_congress("abc"),
               regexp = "Must provide the Congress number as a positive whole number.",
               fixed = TRUE)
  expect_error(year_of_congress(10.5),
               regexp = "Must provide the Congress number as a positive whole number.",
               fixed = TRUE)
  expect_error(year_of_congress(-3.14),
               regexp = "Must provide the Congress number as a positive whole number.",
               fixed = TRUE)
  expect_error(year_of_congress(0),
               regexp = paste("Invalid Congress number (0).",
                              "The Congress number must be a positive whole number."),
               fixed = TRUE)
  expect_error(year_of_congress(-50),
               regexp = paste("Invalid Congress number (-50).",
                              "The Congress number must be a positive whole number."),
               fixed = TRUE)

  # warnings
  expect_warning(expect_equal(year_of_congress(1789), 5365),
                 regexp = paste("That Congress number looks more like a year.",
                                "Did you mean `congress_in_year(1789)`?"),
                 fixed = TRUE)
  expect_warning(expect_equal(year_of_congress(2010), 5807),
                 regexp = paste("That Congress number looks more like a year.",
                                "Did you mean `congress_in_year(2010)`?"),
                 fixed = TRUE)
})

test_that("current congress", {
  curr_cong <- current_congress()
  # check that current congress is integer
  expect_equal(curr_cong, as.integer(curr_cong))

  # matches congress_in_year()
  expect_equal(curr_cong, congress_in_year(as.numeric(format(Sys.Date(), "%Y"))))
  expect_equal(curr_cong, congress_in_year(Sys.Date()))

  expect_gte(curr_cong, 118)
  # reasonable upper bound on current congress
  # (this will fail in the year 2187)
  expect_lt(curr_cong, 200)
})
