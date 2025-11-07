test_that("there are profinit cols", {
  expected_vals <- c(
    `red` = "#E63C41",
    `blue` = "#465a9b",
    `grey` = "#282828",
    `dark blue` = "#2B436C",
    `purple` = "#7D4191",
    `pink` = "#B5578D",
    `yellow` = "#FFD21E",
    `orange` = "#F3943B",
    `green` = "#41C34B",
    `dark green` = "#007938",
    `azure` = "#3DADE5"
  )
  expect_equal(profinit_cols(), expected_vals)
})


test_that("recieve color hex code by name", {
  expect_equal(object = profinit_cols(1, named = TRUE), expected = c("red" = "#E63C41"))
})


test_that("recieve color hexcode by order", {
  expect_equal(object = profinit_cols(2, named = TRUE), expected = c("blue" = "#465a9b"))
})


test_that("recieve multiple hex codes", {
  observed_val <- length(profinit_cols(c(1,2,3)))
  expect_equal(object = observed_val, expected = 3)
})


test_that("palette is function", {
  expect_true(is.function(profinit_pal()))
})


test_that("palette provides desired num. of colors", {
  moje_pal <- profinit_pal()
  barvy <- moje_pal(3)
  expect_equal(object = length(barvy), expected = 3)

  zacinaji_krizkem <- all(substr(barvy, 0, 1) == "#")
  expect_true(zacinaji_krizkem)

  expect_equal(object = nchar(barvy[1]), 7)
})
