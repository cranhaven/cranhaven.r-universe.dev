test_that("Output is a matrix", {
  expect_matrix(check_input(input = data.frame(a = c(3, 4), b = c(5.6, 12.345))))
})

data_dummys <- data.frame(a = c(14, 1, 17.3),
                          b = factor(c("A", "C", "C")),
                          c = factor(c("Male", "Female", "Baby")))
levels_b <- length(levels(data_dummys$b))
levels_c <- length(levels(data_dummys$c))

test_that("Dummys added if there is a factor variable", {
  expect_matrix(check_input(data_dummys), ncols = 1 + levels_b + levels_c)
})

data_list <- list(a = c(14, 1, 17.3),
                  b = factor(c("A", "C", "C")),
                  c = factor(c("Male", "Female", "Baby")))
test_that("List is not an allowed input", {
  expect_error(check_input(data_list))
})
