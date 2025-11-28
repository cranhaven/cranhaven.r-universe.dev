test_that("output classes are correct", {
  testthat::skip_on_cran()
  my_summary <- Hmisc:::summary.formula(Species ~ .,
                                        data = iris, method = "reverse"
  )

  expect_s3_class(tidy_summary(my_summary), "tbl_df")
  expect_s3_class(tidy_summary(my_summary), "data.frame")
})




old_opt <- options(datadist = "dd")
on.exit(options(old_opt))

n <- 1000L
age <- 50L + 12L * rnorm(n)
sex <- factor(sample(c("m", "f"), n, rep = TRUE, prob = c(.6, .4)))

cens <- 15L * runif(n)
h <- .02 * exp(.04 * (age - 50L) + .8 * (sex == "f"))
dt <- -log(runif(n)) / h
e <- ifelse(dt <= cens, 1L, 0L)
dt <- pmin(dt, cens)


test_that("correct class", {
  skip_on_cran()

  dd <- rms::datadist(age, sex)
  options(datadist = "dd")
  s <- survival::Surv(dt, e)
  f <- rms::cph(s ~ rms::rcs(age, 4L) + sex)

  my_summary <- summary(f)

  expect_s3_class(tidy_summary(my_summary), "data.frame")
})









test_that("tidy_summary return var names too (#17)", {

  result <- tidy_summary(
    summary(Species ~ ., data = iris, method = "reverse")
  )

  expect_equal(
    result[[1L]],
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

})
