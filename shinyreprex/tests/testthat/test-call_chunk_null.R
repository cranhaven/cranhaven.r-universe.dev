test_that("When NULL value is parsed from a call, it returns the call in a Repro object", {
  null_call <- str2lang("datasets::penguins")
  class(null_call) <- c("NULL", class(null_call))

  null_call_repro <- repro_call_chunk(null_call)

  expect_s7_class(null_call_repro, Repro)
  expect_identical(null_call_repro@code, list(null_call))
})

test_that("Able to extract a `NULL` variable", {
  test_server <- function(input, output, session) {
    summary_tbl <- reactive(datasets::penguins)
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(min_width = 3.5)

      repro_code <- reprex_reactive(summary_tbl)
      expect_identical(repro_code, "datasets::penguins")
    }
  )
})
