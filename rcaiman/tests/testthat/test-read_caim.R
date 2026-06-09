test_that("read_caim() assigns good layer names", {
  expect_setequal(names(read_caim()), c("Red", "Green", "Blue"))
})
