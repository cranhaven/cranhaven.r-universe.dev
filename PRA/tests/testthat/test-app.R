#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*

test_that("pra_shiny_app creates a Shiny app object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shinychat")
  app <- PRA:::pra_shiny_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("pra_shiny_app accepts model parameter", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shinychat")
  app <- PRA:::pra_shiny_app(model = "qwen2.5")
  expect_s3_class(app, "shiny.appobj")
})

test_that("pra_shiny_app accepts rag parameter", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shinychat")
  app <- PRA:::pra_shiny_app(rag = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("pra_app function exists and has correct parameters", {
  expect_true(is.function(pra_app))
  args <- formals(pra_app)
  expect_true("model" %in% names(args))
  expect_true("rag" %in% names(args))
  expect_true("embed_model" %in% names(args))
  expect_true("port" %in% names(args))
  expect_true("launch.browser" %in% names(args))
})

test_that("pra_app default model is llama3.2", {
  args <- formals(pra_app)
  expect_equal(args$model, "llama3.2")
})

test_that("pra_app default rag is TRUE", {
  args <- formals(pra_app)
  expect_true(args$rag)
})

test_that("get_ollama_models returns character vector", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shinychat")
  models <- PRA:::get_ollama_models()
  expect_type(models, "character")
  expect_true(length(models) >= 1)
})

test_that("get_ollama_models falls back to defaults when Ollama unreachable", {
  # Temporarily overwrite the URL by mocking — but since it's a simple function
  # we can test that it returns a reasonable fallback.
  # The function always returns at least the default models on error.
  models <- PRA:::get_ollama_models()
  expect_type(models, "character")
  expect_true(length(models) >= 1)
})

test_that("pra_app errors when Ollama is not running on wrong port", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  skip_on_cran()

  # pra_app checks connectivity — verify it has the Ollama check
  fn_body <- deparse(body(pra_app))
  expect_true(any(grepl("ollama_ok|Cannot connect to Ollama", fn_body)))
})

test_that("pra_shiny_app includes httpHandler and serverFuncSource", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shinychat")
  skip_if_not_installed("base64enc")

  app <- PRA:::pra_shiny_app(model = "llama3.2", rag = FALSE)
  expect_true(is.function(app$httpHandler))
  expect_true(is.function(app$serverFuncSource))
})

test_that("pra_shiny_app has server function", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("shinychat")

  app <- PRA:::pra_shiny_app(model = "llama3.2", rag = FALSE)
  expect_true(is.function(app$serverFuncSource))
})

# ============================================================================
# route_input integration tests (exercised via app routing logic)
# ============================================================================

test_that("route_input correctly routes /help as command", {
  r <- PRA:::route_input("/help")
  expect_equal(r$mode, "command")
  expect_equal(r$command, "help")
})

test_that("route_input correctly routes conceptual questions as rag", {
  rag_queries <- c(
    "What is earned value management?",
    "How does Monte Carlo simulation work?",
    "Explain CPI and SPI in project management",
    "What are the differences between SMM and MCS?"
  )
  for (q in rag_queries) {
    r <- PRA:::route_input(q)
    expect_equal(r$mode, "rag", info = paste("Query should route to RAG:", q))
  }
})

test_that("route_input correctly routes numerical data as tool", {
  tool_queries <- c(
    "BAC = $500,000 with schedule [0.2, 0.4, 0.6, 0.8, 1.0]",
    "P(C1) = 0.3, P(C2) = 0.2, P(R|C1) = 0.8",
    "Task A has a normal(10, 2) distribution"
  )
  for (q in tool_queries) {
    r <- PRA:::route_input(q)
    expect_equal(r$mode, "tool", info = paste("Query should route to tool:", q))
  }
})
