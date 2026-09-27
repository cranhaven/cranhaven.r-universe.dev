test_that("When a reactiveVal is passed to repro_chunk, the call is handled as .__reactval", {
  my_react_val <- shiny::reactiveVal(1L)
  react_val_call <- str2lang("my_react_val()")
  repro_react_val <- shiny::isolate(repro_chunk(react_val_call))

  expect_repro <- shiny::isolate(repro_call_chunk@methods$.__reactval(react_val_call))
  expect_identical(repro_react_val, expect_repro)
})

test_that("When reactive is passed to repro_chunk, the call is handled as .__reactive", {
  summary_tbl <- shiny::reactive({
    aggregate(
      Sepal.Width ~ Species,
      data = iris,
      FUN = "median"
    )
  })
  react_call <- str2lang("summary_tbl()")
  repro_react <- shiny::isolate(repro_chunk(react_call))

  expect_repro <- shiny::isolate(repro_call_chunk@methods$.__reactive(react_call))
  expect_identical(repro_react, expect_repro)
})

test_that("When a reactive is passed to a module for repro_chunk, the call uses the correct environment", {
  my_module <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      summary_tbl <- shiny::reactive({
        aggregate(
          Sepal.Width ~ Species,
          data = iris,
          FUN = "median"
        )
      })

      summary_repro <- sub_module("sub", summary_tbl)
    })
  }

  sub_module <- function(id, summary_tbl) {
    shiny::moduleServer(id, function(input, output, session) {
      my_reactive <- reactive({
        summary_tbl()
      })

      repro_chunk(str2lang("summary_tbl()"))
    })
  }

  shiny::testServer(my_module, {
    react_call <- str2lang("summary_tbl()")
    expect_repro <- repro_chunk(react_call)
    expect_identical(summary_repro, expect_repro)
  })
})
