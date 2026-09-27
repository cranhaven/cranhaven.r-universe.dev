test_that("When a non-reactive call is passed to reprex_reactive, error gets returned", {
  test_server <- function(input, output, session) {
    test_reactive <- reactive(input$foo)
  }

  shiny::testServer(
    test_server,
    expr = {
      expect_error(
        reprex_reactive(iris),
        "Unable to generate reproducible code for iris, must be an unevaluated reactive object",
        fixed = TRUE
      )
    }
  )
})

test_that("When a reactive is evaluated into reprex_reactive, specific error is returned to user", {
  test_server <- function(input, output, session) {
    test_reactive <- reactive(input$foo)
  }

  shiny::testServer(
    test_server,
    expr = {
      expect_error(
        reprex_reactive(test_reactive()),
        "test_reactive has already been evaluated, please remove brackets to pass through reactive object",
        fixed = TRUE
      )
    }
  )
})

test_that("Able to reproduce a simple one-line reactive", {
  test_server <- function(input, output, session) {
    test_reactive <- reactive(input$foo)
  }

  shiny::testServer(
    test_server,
    expr = {
      repro_code <- reprex_reactive(test_reactive)
      expect_identical(repro_code, "")

      session$setInputs(foo = "bar")

      repro_code <- reprex_reactive(test_reactive)
      expect_identical(repro_code, "\"bar\"")
    }
  )
})

test_that("Able to reproduce a reactive stemming from another reactive", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Sepal.Width > input$min_width), ])

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris_filt(),
        FUN = get(input$summary_fn)
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 3.5, summary_fn = "median")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        paste(
          "iris_filt <- iris[with(iris, Sepal.Width > 3.5), ]",
          "",
          "aggregate(Sepal.Width ~ Species, data = iris_filt, FUN = get(\"median\"))",
          sep = "\n"
        )
      )
    }
  )
})

test_that("When one non-standard package is used, it is added to the top of the script", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Sepal.Width > input$min_width), ])

    summary_tbl <- reactive({
      purrr::map(
        head(names(iris), 4),
        dat = iris_filt(),
        fn = input$summary_fn,
        \(x, dat, fn) {
          aggregate(
            as.formula(paste(x, "~ Species")),
            data = dat,
            FUN = get(fn)
          )
        }
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 3.5, summary_fn = "median")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        paste(
          "library(purrr)",
          "",
          "iris_filt <- iris[with(iris, Sepal.Width > 3.5), ]",
          "",
          "purrr::map(",
          "  head(names(iris), 4),",
          "  dat = iris_filt,",
          "  fn = \"median\",",
          "  function(x, dat, fn) {",
          "    aggregate(as.formula(paste(x, \"~ Species\")), data = dat, FUN = get(fn))",
          "  }",
          ")",
          sep = "\n"
        )
      )

      repro_result <- eval(parse(text = repro_code), envir = new.env())
      expect_s3_class(iris_filt, "reactive")
      expect_identical(repro_result, summary_tbl())
    }
  )
})

test_that("Able to reproduce a reactive using the session user data", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Sepal.Width > session$userData$min_width), ])

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris_filt(),
        FUN = get(input$summary_fn)
      )
    })
  }

  session <- shiny::MockShinySession$new()
  session$userData$min_width <- 3.5

  shiny::testServer(
    test_server,
    session = session,
    expr = {
      session$setInputs(summary_fn = "median")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        paste(
          "iris_filt <- iris[with(iris, Sepal.Width > 3.5), ]",
          "",
          "aggregate(Sepal.Width ~ Species, data = iris_filt, FUN = get(\"median\"))",
          sep = "\n"
        )
      )
    }
  )
})

test_that("Able to reproduce a reactive without printing the environment variable in reactive", {
  test_server <- function(input, output, session) {
    dummy_fn <- "median"

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris,
        FUN = get(dummy_fn)
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        paste(
          "dummy_fn <- \"median\"",
          "",
          "aggregate(Sepal.Width ~ Species, data = iris, FUN = get(dummy_fn))",
          sep = "\n"
        )
      )
    }
  )
})

test_that("Able to reproduce a complex reactive without printing the environment variable in reactive", {
  test_server <- function(input, output, session) {
    dummy_data <- iris

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = dummy_data,
        FUN = mean
      )
    })
  }

  iris_repro <- paste("dummy_data <-", constructive::construct(iris, one_liner = TRUE)$code) |>
    str2lang() |>
    constructive::deparse_call() |>
    paste(collapse = "\n")

  shiny::testServer(
    test_server,
    expr = {
      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        paste(
          iris_repro,
          "",
          "aggregate(Sepal.Width ~ Species, data = dummy_data, FUN = mean)",
          sep = "\n"
        )
      )
    }
  )
})

test_that("Able to reproduce a reactive without printing the environment variable in reactive", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris,
        FUN = get(Sys.getenv("DUMMY_FN"))
      )
    })
  }

  Sys.setenv(DUMMY_FN = "median")
  on.exit(Sys.unsetenv("DUMMY_FN"), add = TRUE)

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(summary_fn = "median")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        "aggregate(Sepal.Width ~ Species, data = iris, FUN = get(Sys.getenv(\"DUMMY_FN\")))"
      )
    }
  )
})

test_that("When reproducing a reactive with multiple dependency reactives, similar variables are not overriding", {
  min_value <- 6

  reactive_1 <- shiny::reactive({
    min_value <- 1.5
    subset(iris, Petal.Width >= min_value)
  })

  reactive_2 <- shiny::reactive({
    subset(mtcars, cyl >= min_value)
  })

  reactive_3 <- shiny::reactive({
    nrow(reactive_1()) * nrow(reactive_2())
  })

  repro_r1 <- shiny::isolate(reprex_reactive(reactive_1))
  expect_no_match(repro_r1, "min_value <- 6")

  repro_r3 <- shiny::isolate(reprex_reactive(reactive_3))
  expect_match(repro_r3, "min_value <- 6")
  remove(min_value, reactive_1, reactive_2, reactive_3)

  this_env <- environment()
  expect_silent(rlang::parse_exprs(repro_r3) |> purrr::walk(rlang::eval_bare, env = this_env))
  expect_identical(nrow(reactive_1) * nrow(reactive_2), 64L * 21L)
})

test_that("When a reactive feeds is bound by an event, the reprex only updates when the reactive updates", {
  test_server <- function(input, output, session) {
    my_name <- reactive(input$name) |>
      bindEvent(input$button, ignoreInit = TRUE)

    test_reactive <- reactive(reprex_reactive(my_name)) |>
      bindEvent(my_name())
  }

  shiny::testServer(
    test_server,
    expr = {
      expect_error(my_name())
      expect_error(test_reactive())

      session$setInputs(name = "Name", button = 1L)
      expect_identical(test_reactive(), "\"Name\"")

      session$setInputs(name = "Not My Name")
      expect_identical(test_reactive(), "\"Name\"")

      session$setInputs(button = 2L)
      expect_identical(test_reactive(), "\"Not My Name\"")
    }
  )
})

test_that("Able to reproduce a reactive using [[ notation for inputs and reactiveValues", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Petal.Width > input[["min_width"]]), ])

    rv <- reactiveValues(summary_fn = "mean")

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris_filt(),
        FUN = get(rv[["summary_fn"]])
      )
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 0.5)

      repro_code <- reprex_reactive(summary_tbl)
      expect_match(repro_code, "iris_filt <-")
      expect_match(repro_code, "0.5")
      expect_match(repro_code, "summary_fn <- \"mean\"")
    }
  )
})

test_that("Reproducible reactives aren't rendered twice when referenced twice in a reactive", {
  reactiveTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      iris_react <- reactive(iris[iris$Species == "versicolor", ])

      table_code <- reactive({

        sum(
          iris_react()$Sepal.Width,
          iris_react()$Sepal.Length,
          na.rm = anyNA(iris_react())
        )

        widths * lengths
      })

      table_reprex <- reactive(reprex_reactive(table_code))

      output$code <- shiny::renderText(table_reprex())
      output$table <- shiny::renderTable(table_code())
    })
  }

  shiny::testServer(
    reactiveTabServer,
    expr = {
      expect_match(table_reprex(), "iris_react <-")
      expect_no_match(table_reprex(), "iris_react <-.*iris_react <-")
    }
  )
})
