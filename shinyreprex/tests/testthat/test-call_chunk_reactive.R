test_that("Reactive call chunk brings back code required to evaluate reactive", {
  summary_tbl <- shiny::reactive({
    aggregate(
      Sepal.Width ~ Species,
      data = iris,
      FUN = "median"
    )
  })

  reactive_call <- str2lang("summary_tbl()")
  class(reactive_call) <- c(".__reactive", class(reactive_call))

  repro_reactive <- shiny::isolate(repro_call_chunk(reactive_call))
  expect_s7_class(repro_reactive, Repro)

  fn_call <- "summary_tbl <- aggregate(Sepal.Width ~ Species, data = iris, FUN = \"median\")"
  expect_identical(repro_reactive@code, list(str2lang(fn_call)))
  expect_identical(repro_reactive@calls, fn_call)
})

test_that("Reactive call chunk can be evaluated when more than 1 call is in expression", {
  summary_tbl <- shiny::reactive({
    iris_filt <- subset(iris, Petal.Width > 1.3)
    iris_med <- aggregate(
      Sepal.Width ~ Species,
      data = iris,
      FUN = "median"
    )
    t(iris_med)
  })

  reactive_call <- str2lang("summary_tbl()")
  class(reactive_call) <- c(".__reactive", class(reactive_call))

  fn_call <- c(
    "summary_tbl <- local(",
    "  {",
    "    iris_filt <- subset(iris, Petal.Width > 1.3)",
    "    iris_med <- aggregate(Sepal.Width ~ Species, data = iris, FUN = \"median\")",
    "    t(iris_med)",
    "  }",
    ")"
  )

  repro_reactive <- shiny::isolate(repro_call_chunk(reactive_call))
  expect_identical(repro_reactive@calls, fn_call)
})

test_that("Reactive call chunk can be evaluated with single package calls", {
  summary_tbl <- shiny::reactive({
    x_expr <- rlang::parse_expr("3L")
    rlang::eval_bare(x_expr)
  })

  reactive_call <- str2lang("summary_tbl()")
  class(reactive_call) <- c(".__reactive", class(reactive_call))

  repro_reactive <- shiny::isolate(repro_call_chunk(reactive_call))
  expect_identical(repro_reactive@packages, "rlang")
  expect_identical(repro_reactive@calls[1L:2L], c("library(rlang)", ""))
})

test_that("Reactive call chunk can be evaluated with multiple package calls", {
  summary_tbl <- shiny::reactive({
    x_expr <- rlang::parse_expr("3L")
    constructive::construct(x_expr)
  })

  reactive_call <- str2lang("summary_tbl()")
  class(reactive_call) <- c(".__reactive", class(reactive_call))

  repro_reactive <- shiny::isolate(repro_call_chunk(reactive_call))
  expect_identical(repro_reactive@packages, c("rlang", "constructive"))
  expect_identical(
    repro_reactive@calls[1L:3L], c("library(rlang)", "library(constructive)", "")
  )
})

test_that("Reactive call chunk can be evaluated when a reactive is called within the reactive", {
  summary_tbl <- shiny::reactive({
    iris_filt <- subset(iris, Petal.Width > 1.3)
    iris_med <- aggregate(
      Sepal.Width ~ Species,
      data = iris,
      FUN = "median"
    )
    t(iris_med)
  })

  summary_result <- shiny::reactive({
    x <- summary_tbl()[1L, 2L]
    round(as.numeric(x))
  })

  reactive_call <- str2lang("summary_result()")
  class(reactive_call) <- c(".__reactive", class(reactive_call))

  repro_reactive <- shiny::isolate(repro_call_chunk(reactive_call))

  summary_call <- c(
    "summary_result <- local({",
    "    x <- summary_tbl[1L, 2L]",
    "    round(as.numeric(x))",
    "  })"
  )

  expect_equal(
    repro_reactive@code,
    list(str2lang(paste(summary_call, collapse = "\n"))),
    ignore_attr = TRUE
  )

  tbl_call <- c(
    "summary_tbl <- local(",
    "  {",
    "    iris_filt <- subset(iris, Petal.Width > 1.3)",
    "    iris_med <- aggregate(Sepal.Width ~ Species, data = iris, FUN = \"median\")",
    "    t(iris_med)",
    "  }",
    ")"
  )

  expect_named(repro_reactive@prerequisites, "summary_tbl")
  expect_equal(
    repro_reactive@prerequisites$summary_tbl,
    list(str2lang(paste(tbl_call, collapse = "\n"))),
    ignore_attr = TRUE
  )

  expect_identical(repro_reactive@calls, c(tbl_call, "", summary_call))
})
