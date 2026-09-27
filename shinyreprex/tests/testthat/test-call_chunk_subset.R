test_that("Subset chunk maintains the same for a static value like regular R objects", {
  subset_call <- str2lang("iris$Sepal.Width")
  class(subset_call) <- c("$", class(subset_call))

  repro_subset <- repro_call_chunk(subset_call)
  expect_s7_class(repro_subset, Repro)
  expect_identical(repro_subset@code, list(str2lang("iris$Sepal.Width")))
  expect_identical(repro_subset@calls, "iris$Sepal.Width")
})

test_that("Subset chunk evaluates an input call and stores in Repro object", {
  subset_call <- str2lang("input$width_range")
  class(subset_call) <- c("$", class(subset_call))

  session <- shiny::MockShinySession$new()
  session$setInputs(width_range = c(4, 6.5))

  repro_subset <- shiny::isolate(repro_call_chunk(subset_call, env = session))
  expect_s7_class(repro_subset, Repro)
  expect_identical(repro_subset@code, list(str2lang("c(4, 6.5)")))
  expect_identical(repro_subset@calls, "c(4, 6.5)")
})

test_that("Subset chunk evaluates an reactiveValue and stores assignment in Repro object", {
  subset_call <- str2lang("rv$width_range")
  class(subset_call) <- c("$", class(subset_call))
  rv <- shiny::reactiveValues(width_range = c(4, 6.5))

  repro_subset <- shiny::isolate(repro_call_chunk(subset_call))
  expect_s7_class(repro_subset, Repro)
  expect_identical(repro_subset@code, list(str2lang("width_range <- c(4, 6.5)")))
  expect_identical(repro_subset@calls, "width_range <- c(4, 6.5)")
})

test_that("Double bracket subset chunk maintains the same for a static R object", {
  subset_call <- str2lang('iris[["Sepal.Width"]]')
  class(subset_call) <- c("[[", class(subset_call))

  repro_subset <- repro_call_chunk(subset_call)
  expect_s7_class(repro_subset, Repro)
  expect_identical(repro_subset@code, list(str2lang('iris[["Sepal.Width"]]')))
  expect_identical(repro_subset@calls, 'iris[["Sepal.Width"]]')
})

test_that("Double bracket subset chunk evaluates an input call and stores in Repro object", {
  subset_call <- str2lang('input[["width_range"]]')
  class(subset_call) <- c("[[", class(subset_call))

  session <- shiny::MockShinySession$new()
  session$setInputs(width_range = c(4, 6.5))

  repro_subset <- shiny::isolate(repro_call_chunk(subset_call, env = session))
  expect_s7_class(repro_subset, Repro)
  expect_identical(repro_subset@code, list(str2lang("c(4, 6.5)")))
  expect_identical(repro_subset@calls, "c(4, 6.5)")
})

test_that("Double bracket subset chunk evaluates a reactiveValues item and stores assignment in Repro object", {
  subset_call <- str2lang('rv[["width_range"]]')
  class(subset_call) <- c("[[", class(subset_call))
  rv <- shiny::reactiveValues(width_range = c(4, 6.5))

  repro_subset <- shiny::isolate(repro_call_chunk(subset_call))
  expect_s7_class(repro_subset, Repro)
  expect_identical(repro_subset@code, list(str2lang("width_range <- c(4, 6.5)")))
  expect_identical(repro_subset@calls, "width_range <- c(4, 6.5)")
})
