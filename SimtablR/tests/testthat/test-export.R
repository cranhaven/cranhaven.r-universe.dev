library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("as_flextable renders simtab_result as flextable", {
  skip_if_not_installed("flextable")
  tab <- suppressMessages(table1(epitabl, vars = "age", by = "adjudicated_acs"))
  ft <- flextable::as_flextable(tab)
  expect_s3_class(ft, "flextable")
})

test_that("as_gt renders simtab_result as gt table", {
  skip_if_not_installed("gt")
  tab <- suppressMessages(table1(epitabl, vars = "age", by = "adjudicated_acs"))
  gt_tbl <- as_gt(tab)
  expect_s3_class(gt_tbl, "gt_tbl")
})

test_that("export_docx and export_xlsx write valid tabular files", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("openxlsx")
  tab <- suppressMessages(table1(epitabl, vars = "age", by = "adjudicated_acs"))
  docx_file <- tempfile(fileext = ".docx")
  xlsx_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(docx_file, xlsx_file)), add = TRUE)
  export_docx(tab, docx_file)
  export_xlsx(tab, xlsx_file)
  expect_true(file.exists(docx_file) && file.info(docx_file)$size > 0)
  expect_true(file.exists(xlsx_file) && file.info(xlsx_file)$size > 0)
})

test_that("autoplot generates ggplot2 forest plot for model results", {
  skip_if_not_installed("ggplot2")
  tab <- suppressMessages(regtab(epitabl, outcomes = "adjudicated_acs", predictors = ~ age + sex, family = stats::binomial("logit")))
  p <- ggplot2::autoplot(tab)
  expect_s3_class(p, "ggplot")
})

test_that("journal_style applies styling rules without altering raw evidence", {
  tab <- suppressMessages(table1(epitabl, vars = "age", style = "nejm"))
  expect_identical(tab$spec$style, "nejm")
  expect_type(tab$data$age$summary$Overall, "double")
})

test_that("export functions validate table format and handle missing dependencies cleanly", {
  expect_error(export_docx(list(a = 1), tempfile(fileext = ".docx")))
})


test_that("export writers reject inputs that are not tables instead of writing empty files", {
  skip_if_not_installed("openxlsx")
  path <- withr::local_tempfile(fileext = ".xlsx")
  expect_error(export_xlsx(NULL, path), class = "simtab_error_input")
  expect_false(file.exists(path))
  expect_error(export_xlsx(list(a = 1), path), class = "simtab_error_input")

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  docx <- withr::local_tempfile(fileext = ".docx")
  expect_error(export_docx(NULL, docx), class = "simtab_error_input")
  expect_false(file.exists(docx))
})
