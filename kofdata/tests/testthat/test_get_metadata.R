context("get_metadata")

test_that("get_metadata works", {
  some_meta <- get_metadata(c("ch.kof.bau.run1.ng08.f12.q_cb_restrict_none.ans_count.d11",
                              "ch.kof.bau.run1.ng08.f12.q_cb_restrict_none.ans_count.d12",
                              "ch.kof.bau.run1.ng08.f12.q_cb_restrict_none.ans_count"))

  expect_equal(length(some_meta), 3)
})

test_that("get_metadata returns NA for nonexistent keys", {
  expect_true(is.na(get_metadata("nokey")[["nokey"]]))
})

test_that("get_metadata returns a list", {
  expect_is(get_metadata("ch.kof.inu.ng08.fx.sector_kof.cm.q_ql_chg_price_sales_p3m.share_eq")[[1]], "list")
})

test_that("get_metadata errors on invalid locale", {
  expect_error(get_metadata(locale="fi"))
})
