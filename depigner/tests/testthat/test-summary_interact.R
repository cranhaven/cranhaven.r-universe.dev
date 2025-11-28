old_opt <- options(datadist = "dd")
on.exit(options(old_opt))

data("transplant", package = "survival")
transplant <- transplant[transplant[["event"]] != "censored", ] %>%
  droplevels()

test_that("expectation class", {
  # strong assignment because the scope of test <https://goo.gl/LJn9rF>
  dd <<- rms::datadist(transplant)

  lrm_mod <- rms::lrm(
    event ~ rms::rcs(age, 3L) * (sex + abo) + rms::rcs(year, 3L),
    data = transplant,
    model = TRUE,
    x = TRUE, y = TRUE
  )

  expect_s3_class(summary_interact(lrm_mod, age, sex), "data.frame")
  expect_s3_class(
    summary_interact(lrm_mod, age, sex, ref_min = 60L, ref_max = 80L),
    "data.frame"
  )
  expect_s3_class(
    summary_interact(
      lrm_mod, age, sex, ref_min = 60L, ref_max = 80L, digits = 5L
    ),
    "data.frame"
  )
  expect_s3_class(summary_interact(lrm_mod, age, abo), "data.frame")
  expect_s3_class(
    summary_interact(lrm_mod, age, abo, level = c("A", "AB")),
    "data.frame"
  )
  expect_s3_class(summary_interact(lrm_mod, age, abo, p = TRUE), "data.frame")
})



test_that("expectation throws error if input not an lrm", {
  # strong assignment because the scope of test <https://goo.gl/LJn9rF>
  dd <<- rms::datadist(transplant)

  ols_mod <- rms::ols(
    futime ~ rms::rcs(age, 3L) * (sex + abo) + rms::rcs(year, 3L),
    data = transplant,
    model = TRUE,
    x = TRUE,
    y = TRUE
  )

  expect_usethis_error(
    summary_interact(ols_mod, age, sex),
    "model has to inherits to lrm class"
  )
})



test_that("Without refname in datadist it trows an error", {
  # strong assignment because the scope of test <https://goo.gl/LJn9rF>
  dd <<- rms::datadist(transplant)

  lrm_mod <- rms::lrm(
    event ~ rms::rcs(age, 3L) * (sex + abo) + rms::rcs(year, 3L),
    data = transplant,
    model = TRUE,
    x = TRUE, y = TRUE
  )

  expect_usethis_error(summary_interact(lrm_mod, age, sexx), "datadist")
  expect_usethis_error(summary_interact(lrm_mod, agee, sex), "datadist")
})



test_that("Without datadist it trows an error", {
  options(datadist = NULL)

  lrm_mod <- rms::lrm(
    event ~ rms::rcs(age, 3L) * (sex + abo) + rms::rcs(year, 3L),
    data = transplant,
    model = TRUE,
    x = TRUE, y = TRUE
  )

  expect_usethis_error(summary_interact(lrm_mod, age, sex), "datadist")
})
