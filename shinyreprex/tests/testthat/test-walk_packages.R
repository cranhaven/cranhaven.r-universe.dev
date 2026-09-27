test_that("Every branch of an if contributes, whichever branch would be taken", {
  test_server <- function(input, output, session) {
    tbl <- reactive({
      if (input$numeric) purrr::keep(iris, is.numeric) else styler::style_text("1+1")
    })
  }

  shiny::testServer(test_server, {
    session$setInputs(numeric = TRUE)
    expect_setequal(reprex_packages(tbl), c("purrr", "styler"))

    session$setInputs(numeric = FALSE)
    expect_setequal(reprex_packages(tbl), c("purrr", "styler"))
  })
})

test_that("Every branch of a switch contributes", {
  test_server <- function(input, output, session) {
    tbl <- reactive({
      switch(input$type,
        numeric = purrr::keep(iris, is.numeric),
        styled = styler::style_text("1+1")
      )
    })
  }

  shiny::testServer(test_server, {
    session$setInputs(type = "numeric")
    expect_setequal(reprex_packages(tbl), c("purrr", "styler"))
  })
})

test_that("Packages used inside an inline function are detected", {
  test_server <- function(input, output, session) {
    tbl <- reactive(purrr::map(1, function(x) styler::style_text("1+1")))
  }

  shiny::testServer(test_server, {
    expect_setequal(reprex_packages(tbl), c("purrr", "styler"))
  })
})

test_that("A namespaced function passed as a value is detected", {
  test_server <- function(input, output, session) {
    tbl <- reactive(do.call(purrr::keep, list(iris, is.numeric)))
  }

  shiny::testServer(test_server, {
    expect_identical(reprex_packages(tbl), "purrr")
  })
})

test_that("Packages of a chained reactive are included once", {
  test_server <- function(input, output, session) {
    numeric_cols <- reactive(purrr::keep(iris, is.numeric))
    tbl <- reactive(styler::style_text(deparse(c(nrow(numeric_cols()), ncol(numeric_cols())))))
  }

  shiny::testServer(test_server, {
    expect_setequal(reprex_packages(tbl), c("purrr", "styler"))
  })
})

test_that("Packages are found through bindEvent and bindCache wrappers", {
  test_server <- function(input, output, session) {
    tbl <- reactive(purrr::keep(iris, is.numeric)) |>
      bindCache(input$go) |>
      bindEvent(input$go, ignoreNULL = FALSE)
  }

  shiny::testServer(test_server, {
    session$setInputs(go = 1)
    expect_identical(reprex_packages(tbl), "purrr")
  })
})

test_that("Shiny calls stripped when reproducing code do not pull in shiny", {
  test_server <- function(input, output, session) {
    tbl <- reactive({
      shiny::req(input$missing)
      shiny::validate("always")
      purrr::keep(iris, is.numeric)
    })
  }

  shiny::testServer(test_server, {
    expect_identical(reprex_packages(tbl), "purrr")
  })
})

test_that("An omitted index, as in a row subset, does not stop the walk", {
  test_server <- function(input, output, session) {
    tbl <- reactive(purrr::keep(iris[iris$Petal.Width > input$w, ], is.numeric))
  }

  shiny::testServer(test_server, {
    session$setInputs(w = 1)
    expect_identical(reprex_packages(tbl), "purrr")
  })
})

test_that("A reactive using only base functions reports no packages", {
  test_server <- function(input, output, session) {
    tbl <- reactive(nrow(iris))
  }

  shiny::testServer(test_server, {
    expect_length(reprex_packages(tbl), 0L)
  })
})

test_that("Packages are reported for a reactive using lambda functions", {
  test_server <- function(input, output, session) {
    tbl <- reactive(lapply(1, function(x) purrr::map(x, mean)))
  }

  shiny::testServer(test_server, {
    expect_identical(reprex_packages(tbl), "purrr")
  })
})

test_that("Packages are reported when a lambda body is a bare symbol", {
  test_server <- function(input, output, session) {
    tbl <- reactive(purrr::map(1, function(x) x))
  }

  shiny::testServer(test_server, {
    expect_identical(reprex_packages(tbl), "purrr")
  })
})

test_that("Packages are found in a reactive passed into a module", {
  mod_server <- function(id, incoming) {
    shiny::moduleServer(id, function(input, output, session) {
      tbl <- reactive(styler::style_text(deparse(nrow(incoming()))))
      register_reactives(tbl)
    })
  }

  test_server <- function(input, output, session) {
    numeric_cols <- reactive(purrr::keep(iris, is.numeric))
    mod_server("mod", numeric_cols)
  }

  shiny::testServer(test_server, {
    expect_setequal(reprex_packages(), c("purrr", "styler"))
  })
})
