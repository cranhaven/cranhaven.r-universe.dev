data(valentesnsList)
mymodel_0 <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 0
)

mymodel_0 |>
  td_logit_intercept(covar = "Hispanic")

mymodel_1 <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 0
) +
  "{y0} x Hispanic" +
  "{y1} x Hispanic" +
  "{y2} x Hispanic"
  

init_defm(mymodel_0, force_new = FALSE)
init_defm(mymodel_1, force_new = TRUE)

theta <- c(-1, .5, .2)

expect_equal(
  loglike_defm(mymodel_0, theta),
  loglike_defm(mymodel_1, theta)
)

expect_stdout(print(mymodel_0), "powerset\\s+:\\s+16\n")
expect_stdout(print(mymodel_1), "powerset\\s+:\\s+13840\n")
