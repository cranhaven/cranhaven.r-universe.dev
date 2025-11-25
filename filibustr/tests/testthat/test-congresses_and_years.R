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
               regexp = "The provided year (`1788`) is too early.",
               fixed = TRUE)
  expect_error(congress_in_year(110),
               regexp = "The provided year (`110`) is too early.",
               fixed = TRUE)
  expect_error(congress_in_year(as.Date("1492-09-01")),
               regexp = "The provided year (`1492`) is too early.",
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
               regexp = "Invalid Congress number: `0`",
               fixed = TRUE)
  expect_error(year_of_congress(-50),
               regexp = "Invalid Congress number: `-50`",
               fixed = TRUE)

  # warnings
  expect_warning(expect_equal(year_of_congress(1789), 5365),
                 regexp = "That Congress number (`1789`) looks more like a year.",
                 fixed = TRUE)
  expect_warning(expect_equal(year_of_congress(2010), 5807),
                 regexp = "That Congress number (`2010`) looks more like a year.",
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

test_that("odd-year January", {
  expect_true(is_odd_year_january(as.Date("2001-01-01")))
  expect_true(is_odd_year_january(as.Date("1775-01-25")))
  expect_true(is_odd_year_january(as.Date("9999-01-31")))

  expect_false(is_odd_year_january(as.Date("2021-02-01")))
  expect_false(is_odd_year_january(as.Date("2008-01-10")))
  expect_false(is_odd_year_january(as.Date("9999-12-31")))
})
