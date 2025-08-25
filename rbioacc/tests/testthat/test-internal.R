test_that(".interpolate_Inf", {
  v1 <- c(Inf, Inf, 3, Inf, 5, Inf, Inf, 4, Inf, Inf)
  t1 <- 1:length(v1)
  expect_true(all(rbioacc:::.interpolate_Inf(v1, t1) == c(0,0,3,4,5,14/3, 13/3,4,0,0) ))
})

test_that(".regularize_Inf", {
  expect_true(
    all(rbioacc:::.regularize_Inf(c(Inf, Inf, 3, Inf, 3, Inf, Inf, 3, Inf, Inf)) == rep(3,10) )
  )
})

