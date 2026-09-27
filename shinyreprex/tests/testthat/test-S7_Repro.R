# Repro S7 object ----
test_that("Repro object can be created with defaults", {
  new_repro <- Repro()

  expect_s7_class(new_repro, Repro)

  expect_identical(new_repro@code, list())
  expect_identical(new_repro@packages, character())
  expect_identical(new_repro@prerequisites, list())
  expect_identical(new_repro@script, "")
  expect_null(new_repro@calls)
})

test_that("Repro object can be created with pre-defined code", {
  my_code <- list(
    str2lang("x <- 42"),
    str2lang("my_var <- x + y")
  )
  my_prereqs <- list(
    y = str2lang("y <- 100")
  )

  new_repro <- Repro(
    code = my_code,
    prerequisites = my_prereqs
  )

  expect_s7_class(new_repro, Repro)

  expect_identical(new_repro@code, my_code)
  expect_identical(new_repro@packages, character())
  expect_identical(new_repro@prerequisites, my_prereqs)
  expect_identical(new_repro@script, "y <- 100\n\nx <- 42\nmy_var <- x + y")

  new_repro@calls |>
    purrr::discard(identical, y = "") |>
    purrr::map(rlang::parse_expr) |>
    purrr::walk(rlang::eval_bare, env = environment())
  expect_identical(my_var, 142)
})

# is_new_reactive ----
test_that("New reactives flag when all reactive variables don't exist in existing set", {
  new_vars <- list(a = NULL, b = NULL, c = NULL)

  expect_true(is_new_reactive(new_vars, NULL))
  expect_true(is_new_reactive(new_vars, list()))
  expect_true(is_new_reactive(new_vars, list(x = NULL, y = NULL, z = NULL)))
})

test_that("New reactives flag when some reactive variables don't exist in existing set", {
  new_vars <- list(a = NULL, b = NULL, c = NULL)

  expect_true(is_new_reactive(new_vars, list(a = NULL, y = NULL, z = NULL)))
})

test_that("New reactives don't flag when all reactive variables exist in existing set", {
  new_vars <- list(a = NULL, b = NULL, c = NULL)

  expect_false(is_new_reactive(new_vars, list(a = NULL, b = NULL, c = NULL)))
})

test_that("New reactives don't flag when new set isn't named", {
  new_vars <- list(NULL, NULL, NULL)

  expect_false(is_new_reactive(new_vars, list(x = NULL, y = NULL, z = NULL)))
})
