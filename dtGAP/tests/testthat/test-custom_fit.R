# --- custom fit ---------------------------------------------------------------

test_that("dtGAP accepts pre-built rpart object", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  rp <- rpart::rpart(Outcome ~ ., data = train_covid, method = "class")

  expect_no_error(
    dtGAP(
      fit = rp,
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      print_eval = FALSE
    )
  )
})

test_that("convert_to_party handles party objects as identity", {
  pt <- partykit::ctree(Outcome ~ ., data = toy_train)
  result <- convert_to_party(pt, "party")
  expect_s3_class(result, "party")
})

test_that("convert_to_party converts rpart to party", {
  rp <- rpart::rpart(Outcome ~ ., data = toy_train, method = "class")
  result <- convert_to_party(rp, "rpart")
  expect_s3_class(result, "party")
})

test_that("detect_model_type identifies classes correctly", {
  rp <- rpart::rpart(Outcome ~ ., data = toy_train, method = "class")
  pt <- partykit::ctree(Outcome ~ ., data = toy_train)

  expect_equal(detect_model_type(rp), "rpart")
  expect_equal(detect_model_type(pt), "party")
})

test_that("detect_model_type rejects unsupported objects", {
  fit_lm <- lm(LDH ~ hs_CRP, data = toy_train)
  expect_error(detect_model_type(fit_lm), "Unsupported fit class")
})

test_that("dtGAP accepts user-provided var_imp with fit", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  rp <- rpart::rpart(Outcome ~ ., data = train_covid, method = "class")
  vi <- c(LDH = 0.5, hs_CRP = 0.3, Lymphocyte = 0.2)

  expect_no_error(
    dtGAP(
      fit = rp,
      user_var_imp = vi,
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      print_eval = FALSE
    )
  )
})
