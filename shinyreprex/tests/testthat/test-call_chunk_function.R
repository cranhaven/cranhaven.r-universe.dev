test_that("Able to extract anonymous functions within a reactive expression", {
  test_fn <- shiny::reactive({
    my_sub_fn <- function(y) {
      print("Hello")
      y
    }

    my_sub_fn("Test")
  })

  sub_fn_repro <- repro_chunk(test_fn)

  expect_s7_class(sub_fn_repro, Repro)
  expect_identical(
    deparse1(sub_fn_repro@code[[1L]], collapse = "\n"),
    "my_sub_fn <- function(y) {\n    print(\"Hello\")\n    y\n}"
  )
})

test_that("Able to extract inputs inside anonymous functions within a reactive expression", {
  test_server <- function(input, output, session) {
    max_area <- reactive({
      helper_fn <- function(x) {
        x + 10 * input$min_width
      }

      helper_fn(input$max_width)
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5, max_width = 10)

      repro_code <- reprex_reactive(max_area)
      expect_identical(
        repro_code,
        paste(
          "helper_fn <- function(x) {",
          "  x + 10 * 2.5",
          "}",
          "helper_fn(10)",
          sep = "\n"
        )
      )
    }
  )
})

test_that("Able to extract inputs as arguments to anonymous functions within a reactive expression", {
  test_server <- function(input, output, session) {
    max_area <- reactive({
      helper_fn <- function(x, y = input$min_width) {
        x + 10 * y
      }

      helper_fn(input$max_width)
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 2.5, max_width = 10)

      repro_code <- reprex_reactive(max_area)
      expect_identical(
        repro_code,
        paste(
          "helper_fn <- function(x, y = 2.5) {",
          "  x + 10 * y",
          "}",
          "helper_fn(10)",
          sep = "\n"
        )
      )
    }
  )
})
