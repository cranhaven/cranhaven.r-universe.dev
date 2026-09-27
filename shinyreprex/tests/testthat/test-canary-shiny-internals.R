test_that("canary: shiny reactive exposes observable$.origFunc, with the module as its parent environment", {
  # guards R/chunk_reactive.R
  the_answer <- 42L
  r <- shiny::reactive(the_answer)

  observer <- attr(r, "observable", exact = TRUE)
  expect_true(is.function(observer$.origFunc))
  expect_true(rlang::env_has(rlang::env_parent(environment(observer$.origFunc)), "the_answer"))
})

test_that("canary: bindEvent wraps the reactive as a valueFunc carrying the inner reactive in wrappedFunc attribute", {
  #  guards the unwrap loop in R/chunk_reactive.R
  r <- shiny::bindEvent(shiny::reactive(1 + 1), TRUE)

  observer <- attr(r, "observable", exact = TRUE)
  module_env <- rlang::env_parent(environment(observer$.origFunc))
  expect_true(rlang::env_has(module_env, "valueFunc"))
  expect_true(is.function(attr(module_env$valueFunc, "wrappedFunc", exact = TRUE)))
})
