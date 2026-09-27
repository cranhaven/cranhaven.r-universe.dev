test_that("Able to extract a simple value from a reactiveVal", {
  my_react_val <- shiny::reactiveVal(1L)
  react_val_call <- str2lang("my_react_val()")
  class(react_val_call) <- c(".__reactval", class(react_val_call))

  repro_react_val <- shiny::isolate(repro_call_chunk(react_val_call))

  expect_s7_class(repro_react_val, Repro)
  expect_identical(repro_react_val@code, list(str2lang("my_react_val <- 1L")))
  expect_identical(repro_react_val@calls, "my_react_val <- 1L")
})

test_that("Able to extract a vector from a reactiveVal", {
  my_react_val <- shiny::reactiveVal(letters)
  react_val_call <- str2lang("my_react_val()")
  class(react_val_call) <- c(".__reactval", class(react_val_call))
  letters_str <- paste0("my_react_val <- c(", paste0("\"", letters, "\"", collapse = ", "), ")")

  repro_react_val <- shiny::isolate(repro_call_chunk(react_val_call))
  expect_identical(deparse1(repro_react_val@code[[1]]), letters_str)
})

test_that("Able to extract a data.frame from a reactiveVal", {
  my_react_val <- shiny::reactiveVal(iris)
  react_val_call <- str2lang("my_react_val()")
  class(react_val_call) <- c(".__reactval", class(react_val_call))

  repro_react_val <- shiny::isolate(repro_call_chunk(react_val_call))
  expect_match(deparse1(repro_react_val@code[[1]]), "my_react_val <- data.frame(", fixed = TRUE)
  expect_match(repro_react_val@calls[1], "my_react_val <- data.frame(", fixed = TRUE)
})

test_that("reactiveVal setter call raises a warning and returns empty Repro", {
  my_react_val <- shiny::reactiveVal(1L)
  setter_call <- str2lang("my_react_val(42L)")
  class(setter_call) <- c(".__reactval_setter", class(setter_call))

  expect_warning(
    repro_setter <- shiny::isolate(repro_call_chunk(setter_call)),
    "`my_react_val()` is a reactiveVal setter",
    fixed = TRUE
  )
  expect_s7_class(repro_setter, Repro)
  expect_identical(repro_setter@code, list())
})

test_that("reactiveVal setter is omitted but getter still produces output in a full reactive", {
  test_server <- function(input, output, session) {
    my_val <- shiny::reactiveVal(0)

    test_reactive <- reactive({
      my_val(input$new_value)
      my_val()
    })

    output$my_val <- renderText({
      req(input$set_new_value)
      test_reactive()
    })
  }

  shiny::testServer(
    test_server,
    expr = {
      session$setInputs(new_value = 42L)

      expect_warning(
        repro_code <- reprex_reactive(test_reactive),
        "`my_val()` is a reactiveVal setter",
        fixed = TRUE
      )
      # Setter is skipped; getter reflects the current (pre-update) value
      expect_no_match(repro_code, "my_val(42", fixed = TRUE)
      expect_match(repro_code, "my_val <- 0")

      session$setInputs(set_new_value = TRUE)
      expect_warning(
        repro_code <- reprex_reactive(test_reactive),
        "`my_val()` is a reactiveVal setter",
        fixed = TRUE
      )
      expect_match(repro_code, "my_val <- 42")
    }
  )
})
