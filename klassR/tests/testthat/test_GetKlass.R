# Code for tests for the function get_klass

test_that("get_klass returns a classification", {
  class_data <- get_klass(131, date = "2015-01-01")
  expect_equal(class_data$code[1], "0101")
})


test_that("get_klass returns a classification with Norwegian letters", {
  class_data <- get_klass(131, date = "2015-01-01")
  expect_equal(class_data$name[8], "Rømskog")
})


test_that("get_klass returns no error when no date given", {
  expect_type(get_klass(131), type = "list")
})


test_that("get_klass returns error when date in wrong format", {
  expect_error(
    x <- get_klass(2, date = "01-01-2024"),
    "An incorrect date format was given. Please use format 'YYYY-mm-dd'."
  )

  expect_error(
    x <- get_klass(2, date = "20240101"),
    "An incorrect date format was given. Please use format 'YYYY-mm-dd'."
  )

  expect_error(
    x <- get_klass(2,
      date = c("2024-01-01", "2023-01-01", "2022-01-01"),
      "You have provided too many dates."
    )
  )
})


test_that("get_klass returns changes when two dates given", {
  class_data1 <- get_klass(131, date = c("2022-01-01", "2023-01-01"))
  class_data2 <- get_klass(131, date = c("2023-01-01", "2022-01-01"))
  expect_equal(nrow(class_data1), nrow(class_data2))
})


test_that("get_klass returns classifications with future date", {
  expect_message(
    class_data <- get_klass(2, date = "2100-01-01"),
    "The date you selected is in the future. You may be viewing a future classification that is not currently valid"
  )
  expect_equal(nrow(class_data), 2)
})


test_that("get_klass returns correct output level", {
  class_data <- get_klass(7,
    date = "2024-01-01",
    output_level = 1
  )
  expect_equal(nrow(class_data), 10)
  expect_equal(class_data$name[2], "Ledere")
})


test_that("get_klass returns english language", {
  class_data <- get_klass(2,
    date = "2024-01-01",
    language = "en"
  )
  expect_equal(class_data$name, c("Male", "Female"))
})


test_that("get_klass returns a correspondence table", {
  class_data <- get_klass(104,
    correspond = 131,
    date = "2015-01-01"
  )
  expect_equal(class_data$sourceCode[1], "01")
})


test_that("get_klass returns a correspondence table in both directions", {
  class_data1 <- get_klass(104,
    correspond = 131,
    date = "2020-02-01"
  )
  class_data2 <- get_klass(131,
    correspond = 104,
    date = "2020-02-01"
  )
  expect_equal(class_data1$sourceCode[1], "03")
  expect_equal(class_data2$sourceCode[1], "0301")
})


test_that("get_klass returns a valid variant including ones with spaces in name", {
  variant_data <- get_klass(6, variant = 1616, date = "2021-01-02")
  expect_equal(variant_data$name[2], "Jordbruk, skogbruk og fiske")

  variant_data <- get_klass(6, variant = 1615, date = "2021-01-02")
  expect_equal(variant_data$name[6], "Dyrking av ris")
})

test_that("get_klass returns a correspondence table", {
  cor_data1 <- get_klass(131, correspond = 127, date = "2024-01-02")
  cor_data2 <- get_klass(127, correspond = 131, date = "2024-01-02")

  expect_gt(nrow(cor_data1), 1)

  expect_equal(cor_data1$sourceCode, cor_data2$targetCode)
})

test_that("get_klass returns a correspondent using ID", {
  expect_message(
    cor_data <- get_klass(correspondID = 1441, date = "2024-01-02")
  )
  expect_equal(cor_data$targetCode[1], "3100")
})


test_that("get_klass fails quietly with invalid variant", {
  # Capture the warnings and errors
  err <- NULL
  result <- tryCatch(
    {
      variant_data <- get_klass(6, variant = 1, date = "2021-01-02")
    },
    error = function(e) {
      err <<- e
      NULL
    }
  )

  # Check that an error was indeed thrown
  expect_true(!is.null(err))

  # Check that the error message is empty
  expect_true(nchar(conditionMessage(err)) == 0)
})


test_that("get_klass returns notes when requested", {
  class_data <- get_klass(277, notes = TRUE, date = "2023-01-12")
  note <- class_data$notes[class_data$code == "FGK8"]
  expect_equal(note, "Grupperingen omfatter funksjonene 202, 215, 222 og 223 innen grunnskole.")
})


test_that("get_klass returns a future classification", {
  expect_message(classdata <- get_klass(104, date = "2050-01-01"))
  expect_equal(classdata$name[6], "Akershus")
})


test_that("get_klass returns a correspondence table using ID", {
  expect_message(classdata <- get_klass(correspondID = 1111, date = "2023-01-12"))
  expect_equal(classdata$targetName[1], "Oslo")
})

test_that("get_klass returns valid dates for a date range", {
  class_data <- get_klass(131, date = c("2020-01-01", "2024-01-02"))
  expect_equal(class_data$validFromInRequestedRange[1], "2020-01-01")

  variant_data <- get_klass(6, variant = 1616, date = c("2020-01-01", "2024-01-02"))
  expect_equal(variant_data$validFromInRequestedRange[1], "2020-01-01")
})
