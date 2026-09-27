test_that("Able to extract the 'if' part of an if/else statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) {
        iris[with(iris, Sepal.Width > input$min_width), ]
      } else {
        iris[with(iris, Sepal.Width < input$min_width), ]
      }
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 3.5)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "iris[with(iris, Sepal.Width > 3.5), ]")
    }
  )
})

test_that("Able to extract the 'else' part of an if/else statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) {
        iris[with(iris, Sepal.Width > input$min_width), ]
      } else {
        iris[with(iris, Sepal.Width < input$min_width), ]
      }
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "iris[with(iris, Sepal.Width < 2.5), ]")
    }
  )
})

test_that("Able to extract the 'else if' part of an if/else statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) {
        iris[with(iris, Sepal.Width > input$min_width), ]
      } else if (input$species == "versicolor") {
        iris[with(iris, Species == input$species & Sepal.Width > input$min_width), ]
      } else {
        iris[with(iris, Sepal.Width < input$min_width), ]
      }
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5, species = "versicolor")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "iris[with(iris, Species == \"versicolor\" & Sepal.Width > 2.5), ]")
    }
  )
})

test_that("Able to extract the 'else if' part of an if/else if/else statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) {
        iris[with(iris, Sepal.Width > input$min_width), ]
      } else if (input$species == "versicolor") {
        iris[with(iris, Species == input$species & Sepal.Width > input$min_width), ]
      } else {
        iris[with(iris, Sepal.Width < input$min_width), ]
      }
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5, species = "setosa")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "iris[with(iris, Sepal.Width < 2.5), ]")
    }
  )
})

test_that("Able to extract an unbraced 'if' branch as a single expression", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$numeric_only) purrr::keep(iris, is.numeric) else styler::style_text("1+1")
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(numeric_only = TRUE)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        "library(purrr)\n\npurrr::keep(iris, is.numeric)"
      )
    }
  )
})

test_that("Able to extract an unbraced 'else' branch as a single expression", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$numeric_only) purrr::keep(iris, is.numeric) else purrr::discard(iris, is.numeric)
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(numeric_only = FALSE)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        "library(purrr)\n\npurrr::discard(iris, is.numeric)"
      )
    }
  )
})

test_that("Able to extract an unbraced 'else if' branch as a single expression", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) iris[with(iris, Sepal.Width > input$min_width), ]
      else if (input$species == "versicolor") purrr::keep(iris, is.numeric)
      else iris
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5, species = "versicolor")

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        "library(purrr)\n\npurrr::keep(iris, is.numeric)"
      )
    }
  )
})

test_that("Able to mix braced and unbraced branches in the same if/else statement", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      # Mismatched braces are the subject of this test
      if (input$numeric_only) { # nolint: brace_linter.
        purrr::keep(iris, is.numeric)
      } else styler::style_text("1+1")
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(numeric_only = FALSE)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(
        repro_code,
        "library(styler)\n\nstyler::style_text(\"1+1\")"
      )
    }
  )
})

test_that("Packages are detected within an unbraced branch", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$numeric_only) purrr::keep(iris, is.numeric) else styler::style_text("1+1")
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(numeric_only = TRUE)

      expect_identical(repro_chunk(summary_tbl)@packages, "purrr")
    }
  )
})

test_that("Able to extract the 'if' part of an if statement with no else", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) iris[with(iris, Sepal.Width > input$min_width), ]
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 3.5)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "iris[with(iris, Sepal.Width > 3.5), ]")
    }
  )
})

test_that("Returns empty script when an if statement with no else is not met", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$min_width > 3) iris[with(iris, Sepal.Width > input$min_width), ]
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "")
    }
  )
})

test_that("Able to extract the branch of an if statement with a numeric condition", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$n_species) iris[iris$Species == "setosa", ] else iris
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      # A truthy condition that is not `TRUE` still selects the 'if' branch
      session$setInputs(n_species = 2)
      expect_identical(reprex_reactive(summary_tbl), 'iris[iris$Species == "setosa", ]')

      session$setInputs(n_species = 1)
      expect_identical(reprex_reactive(summary_tbl), 'iris[iris$Species == "setosa", ]')

      session$setInputs(n_species = 0)
      expect_identical(reprex_reactive(summary_tbl), "iris")
    }
  )
})

test_that("Only packages used in the branch taken by the current inputs are detected", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive({
      if (input$numeric_only) purrr::keep(iris, is.numeric) else styler::style_text("1+1")
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(numeric_only = FALSE)
      expect_identical(repro_chunk(summary_tbl)@packages, "styler")

      session$setInputs(numeric_only = TRUE)
      expect_identical(repro_chunk(summary_tbl)@packages, "purrr")
    }
  )
})
