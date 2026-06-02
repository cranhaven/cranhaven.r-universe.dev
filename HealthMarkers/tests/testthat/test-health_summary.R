
test_that("health_summary works on numeric columns", {
  df <- data.frame(a = c(1,2,NA), b = c(3,4,5), c = factor("x"))
  s <- health_summary(df)
  expect_true(all(c("a","b") %in% s$measure))
  expect_true(all(c("mean","sd","median","p25","p75") %in% names(s)))
})