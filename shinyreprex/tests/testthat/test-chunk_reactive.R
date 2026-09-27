# repro_chunk ----
test_that("Reactive code chunk is correctly extracted", {
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
      repro_code <- repro_chunk(summary_tbl)

      expect_s7_class(repro_code, Repro)
      expect_identical(
        repro_code@code,
        list(str2lang("aggregate(Sepal.Width ~ Species, data = iris_filt, FUN = get(\"median\"))"))
      )
      expect_identical(
        repro_code@prerequisites,
        list(
          iris_filt = list(
            str2lang("iris_filt <- iris[with(iris, Sepal.Width > 3.5), ]")
          )
        )
      )
    }
  )
})

# reprex reactives ----
test_that("Able to extract reactive expression code from a standard reactive", {
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

test_that("Able to extract reactive expression code from bindCache", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Sepal.Width > input$min_width), ]) |>
      bindCache(input$min_width)

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris_filt(),
        FUN = get(input$summary_fn)
      )
    }) |>
      bindCache(iris_filt(), input$summary_fn)
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

test_that("Able to extract reactive expression code from bindEvent", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Sepal.Width > input$min_width), ]) |>
      bindEvent(input$min_width)

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris_filt(),
        FUN = get(input$summary_fn)
      )
    }) |>
      bindEvent(iris_filt(), input$summary_fn)
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

test_that("Able to extract reactive expression code from bindEvent and bindCache", {
  test_server <- function(input, output, session) {
    iris_filt <- reactive(iris[with(iris, Sepal.Width > input$min_width), ]) |>
      bindCache(iris_filt(), input$summary_fn) |>
      bindEvent(input$min_width)

    summary_tbl <- reactive({
      aggregate(
        Sepal.Width ~ Species,
        data = iris_filt(),
        FUN = get(input$summary_fn)
      )
    }) |>
      bindCache(iris_filt(), input$summary_fn) |>
      bindEvent(iris_filt(), input$summary_fn)
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

test_that("Able to extract reactive expression code from eventReactive", {
  test_server <- function(input, output, session) {
    iris_filt <- eventReactive(input$min_width, iris[with(iris, Sepal.Width > input$min_width), ])

    summary_tbl <- eventReactive(list(iris_filt(), input$summary_fn), {
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
