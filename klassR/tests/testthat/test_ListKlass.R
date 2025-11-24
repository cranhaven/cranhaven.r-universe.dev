# Test searches for classifications

# library(klassR)

test_that("ListKlass returns a warning but still runs", {
  expect_warning(
    klass_table <- ListKlass()
  )
  expect_true(exists("klass_table"))
})


test_that("list_klass returns a list", {
  klass_table <- list_klass()
  expect_gt(nrow(klass_table), 1)

  codelist_table <- list_klass(codelists = TRUE)
  expect_gt(nrow(codelist_table), nrow(klass_table))
})


test_that("search_klass returns searches", {
  komm_search <- search_klass("kommune")
  expect_in("Standard for kommuneinndeling", komm_search$klass_name)
})
