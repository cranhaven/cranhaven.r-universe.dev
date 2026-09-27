# get_pkg_name ----
test_that("Able to extract package name from pkg::fn call", {
  expect_identical(get_pkg_name(str2lang("rlang::sym('foo')")), "rlang")
})

test_that("Able to extract package name from unnamespaced function call", {
  expect_identical(get_pkg_name(str2lang("repro_chunk('foo')")), "shinyreprex")
})

test_that("When extract package from function call, base R packages are ignored", {
  expect_null(get_pkg_name(str2lang("nzchar('foo')")))
})

test_that("Call checks return FALSE, not NA, when the call has no function name", {
  anon_call <- str2lang('get("mean")(1:10)')

  expect_false(is_reactive_call(anon_call))
  expect_false(is_reactive_val_call(anon_call))
  expect_false(is_reactive_val_setter_call(anon_call))
})

test_that("A reactive calling an unnamed function is reproduced rather than erroring", {
  testthat::skip_if_not_installed("shiny")

  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE), add = TRUE)

  named_fn <- shiny::reactive(get("mean")(1:10))
  expect_identical(reprex_reactive(named_fn), 'get("mean")(1:10)')

  anon_fn <- shiny::reactive((function(x) x + 1)(1))
  expect_identical(reprex_reactive(anon_fn), "(function(x) x + 1)(1)")
})

# is_reactive_call ----
test_that("A function call with no arguments is not classed as a reactive", {
  testthat::skip_if_not_installed("shiny")

  my_fn <- function() "hello"
  my_reactive <- shiny::reactive("")

  expect_false(is_reactive_call(quote(my_fn())))
  expect_true(is_reactive_call(quote(my_reactive())))
})
