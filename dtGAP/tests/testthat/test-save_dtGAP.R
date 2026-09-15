# --- save_dtGAP ---------------------------------------------------------------

test_that("save_dtGAP creates a PNG file", {
  skip_unless_extended()

  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)

  save_dtGAP(
    file = f,
    data_train = train_covid,
    data_test = test_covid,
    target_lab = "Outcome",
    show = "test",
    print_eval = FALSE
  )

  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("save_dtGAP creates a PDF file", {
  skip_unless_extended()

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  save_dtGAP(
    file = f,
    data_train = train_covid,
    data_test = test_covid,
    target_lab = "Outcome",
    show = "test",
    print_eval = FALSE
  )

  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("save_dtGAP creates an SVG file", {
  skip_unless_extended()

  f <- tempfile(fileext = ".svg")
  on.exit(unlink(f), add = TRUE)

  save_dtGAP(
    file = f,
    data_train = train_covid,
    data_test = test_covid,
    target_lab = "Outcome",
    show = "test",
    print_eval = FALSE
  )

  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("save_dtGAP infers format from extension", {
  skip_unless_extended()

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  # Should work without explicit format

  save_dtGAP(
    file = f,
    data_train = train_covid,
    data_test = test_covid,
    target_lab = "Outcome",
    show = "test",
    print_eval = FALSE
  )

  expect_true(file.exists(f))
})

test_that("save_dtGAP errors on unsupported format", {
  expect_error(
    save_dtGAP(
      file = tempfile(fileext = ".bmp"),
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome"
    ),
    "Unsupported format"
  )
})

test_that("save_dtGAP errors when no extension and no format", {
  expect_error(
    save_dtGAP(
      file = tempfile(),
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome"
    ),
    "Cannot infer format"
  )
})

test_that("save_dtGAP errors on missing directory", {
  expect_error(
    save_dtGAP(
      file = file.path(tempdir(), "no_such_dir", "out.png"),
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome"
    ),
    "Directory does not exist"
  )
})
