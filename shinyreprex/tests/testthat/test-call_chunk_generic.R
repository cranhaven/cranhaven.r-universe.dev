test_that("A non-call object cannot be evaluated as a 'call chunk'", {
  expect_error(repro_call_chunk(letters), "Object passed to repro_call_chunk must be a call")
})

test_that("A call object can be evaluated as a 'call chunk' and return a Repro object", {
  call_repro <- repro_call_chunk(str2lang("mean(1:10)"))

  expect_s7_class(call_repro, Repro)
  expect_identical(call_repro@script, "mean(1:10)")
})

test_that("When a package is defined for the function call chunk, that information is stored in the Repro object", {
  sym_call <- "rlang::sym(\"Species\")"

  call_repro <- repro_call_chunk(str2lang(sym_call))
  expect_identical(call_repro@packages, "rlang")
  expect_identical(call_repro@script, paste0("library(rlang)\n\n", sym_call))
})

test_that("When a variable is one of the arguments of a call, it is extracted from the environment", {
  my_env <- new.env()
  my_env$my_var <- 5.5

  sum_call <- "sum(3, my_var, 30)"
  call_repro <- repro_call_chunk(str2lang(sum_call))
  expect_identical(call_repro@script, sum_call)

  call_repro <- repro_call_chunk(str2lang(sum_call), env = my_env)
  expect_named(call_repro@prerequisites, "my_var")
  expect_match(call_repro@script, "my_var <- 5.5.*sum\\(3, my_var, 30\\)")
})

test_that("When a reactive is one of the arguments of a call, its definition is extracted from the environment", {
  my_env <- new.env()
  my_env$my_reactive <- shiny::reactive(subset(iris, Petal.Width > 1))

  aggregate_call <- "aggregate(my_reactive(), . ~ Species, mean)"
  call_repro <- repro_call_chunk(str2lang(aggregate_call))
  expect_identical(call_repro@script, aggregate_call)

  call_repro <- repro_call_chunk(str2lang(aggregate_call), env = my_env)
  expect_named(call_repro@prerequisites, "my_reactive")
  expect_match(call_repro@script, "my_reactive <- subset\\(.*aggregate\\(my_reactive")
})

test_that("When a call chunk is overriding an exisitng variable, that is not included in the Repro object", {
  my_env <- new.env()
  my_env$my_var <- 5.5

  assign_call <- "my_var <- 3"
  call_repro <- repro_call_chunk(str2lang(assign_call), env = my_env)

  expect_length(call_repro@prerequisites, 0L)
  expect_identical(call_repro@script, assign_call)
})
