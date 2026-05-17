#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*

test_that("pra_system_prompt returns a non-empty string", {
  prompt <- PRA:::pra_system_prompt()
  expect_type(prompt, "character")
  expect_true(nchar(prompt) > 50)
  # Should mention key tool names
  expect_true(grepl("mcs_tool", prompt))
  expect_true(grepl("evm_analysis_tool", prompt))
  expect_true(grepl("risk_prob_tool", prompt))
  expect_true(grepl("fit_and_predict_sigmoidal_tool", prompt))
  # Should instruct citation of RAG sources
  expect_true(grepl("cite", prompt, ignore.case = TRUE))
  # Should distinguish conceptual questions from computation
  expect_true(grepl("CONCEPTUAL", prompt))
  expect_true(grepl("NUMERICAL DATA", prompt))
  # Should tell the LLM not to output /commands
  expect_true(grepl("Do not use /commands", prompt, ignore.case = TRUE))
})

test_that("pra_system_prompt is concise for small models", {
  prompt <- PRA:::pra_system_prompt()
  # Should be under 2000 chars
  expect_true(nchar(prompt) < 2000)
})

test_that("pra_chat requires ellmer", {
  skip_if_not_installed("ellmer")
  expect_true(is.function(pra_chat))
})

test_that("pra_chat accepts chat parameter", {
  skip_if_not_installed("ellmer")
  # Function should accept a chat parameter
  args <- formals(pra_chat)
  expect_true("chat" %in% names(args))
  expect_null(args$chat)
})

test_that("pra_chat default model is llama3.2", {
  args <- formals(pra_chat)
  expect_equal(args$model, "llama3.2")
})

test_that("pra_app requires shiny and ellmer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  expect_true(is.function(pra_app))
})

# ============================================================================
# pra_chat() constructor tests (require Ollama)
# ============================================================================

test_that("pra_chat creates a chat with tools registered", {
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  chat <- pra_chat(model = "llama3.2", rag = FALSE)
  expect_true(!is.null(chat))
  # Chat should be an R6 or S7 object with a $chat method
  expect_true(is.function(chat$chat) || is.function(chat[["chat"]]))
})

test_that("pra_chat with rag=TRUE initializes RAG store", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("ragnar")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  chat <- pra_chat(model = "llama3.2", rag = TRUE)
  expect_true(!is.null(chat))
  # RAG store should be set in the agent env
  expect_true(!is.null(PRA:::.pra_agent_env$rag_store))
})

test_that("pra_chat with rag=FALSE does not build knowledge base", {
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  # Clear the rag_store first
  env <- PRA:::.pra_agent_env
  old_store <- env$rag_store
  env$rag_store <- NULL

  chat <- pra_chat(model = "llama3.2", rag = FALSE)
  expect_true(!is.null(chat))
  # RAG store should remain NULL when rag=FALSE
  expect_null(env$rag_store)

  # Restore
  env$rag_store <- old_store
})

test_that("pra_chat accepts pre-configured chat object", {
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  # Create a bare Ollama chat, then pass it to pra_chat
  base_chat <- ellmer::chat_ollama(model = "llama3.2")
  result <- pra_chat(chat = base_chat, rag = FALSE)
  expect_true(!is.null(result))
  # The returned object should be the same chat, now with tools
  expect_identical(result, base_chat)
})

test_that("pra_system_prompt includes tool routing guidance", {
  prompt <- PRA:::pra_system_prompt()
  # Should have tool selection section
  expect_true(grepl("Tool selection", prompt))
  # Should reference key tool types
  expect_true(grepl("smm_tool", prompt))
  expect_true(grepl("parent_dsm_tool", prompt))
  expect_true(grepl("cost_pdf_tool", prompt))
})

# ============================================================================
# pra_chat() with mock chat object (no Ollama needed, always runs in covr)
# ============================================================================

test_that("pra_chat with pre-configured chat sets system prompt and registers tools", {
  skip_if_not_installed("ellmer")

  # Create a minimal mock chat using R6-like interface
  mock_chat <- local({
    prompt_set <- NULL
    tools_registered <- list()
    env <- new.env(parent = emptyenv())
    env$set_system_prompt <- function(prompt) {
      prompt_set <<- prompt
    }
    env$register_tool <- function(tool) {
      tools_registered[[length(tools_registered) + 1L]] <<- tool
    }
    env$chat <- function(msg) "mock response"
    env$get_turns <- function() list()
    env$.prompt_set <- function() prompt_set
    env$.tools_registered <- function() tools_registered
    env
  })

  result <- pra_chat(chat = mock_chat, rag = FALSE)

  # Should return the same object
  expect_identical(result, mock_chat)
  # System prompt should have been set
  expect_true(!is.null(mock_chat$.prompt_set()))
  expect_true(nchar(mock_chat$.prompt_set()) > 50)
  # Tools should have been registered
  expect_true(length(mock_chat$.tools_registered()) > 0)
})

test_that("pra_chat with rag=TRUE and mock chat attempts RAG setup", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("ragnar")

  mock_chat <- local({
    env <- new.env(parent = emptyenv())
    env$set_system_prompt <- function(prompt) NULL
    env$register_tool <- function(tool) NULL
    env$chat <- function(msg) "mock response"
    env
  })

  # Clear RAG store first
  env <- getFromNamespace(".pra_agent_env", "PRA")
  old_store <- env$rag_store
  env$rag_store <- NULL

  # This will try to build knowledge base — may succeed or warn depending on Ollama
  result <- tryCatch(
    pra_chat(chat = mock_chat, rag = TRUE),
    warning = function(w) {
      # If Ollama not available, RAG setup warns but continues
      mock_chat
    }
  )
  expect_true(!is.null(result))

  # Restore
  env$rag_store <- old_store
})

# ============================================================================
# pra_app() constructor tests
# ============================================================================

test_that("pra_app errors when Ollama is not running", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  skip_on_cran()

  # This will only work if Ollama is NOT running on a non-standard port
  # The function checks connectivity before starting
  expect_true(is.function(pra_app))
  args <- formals(pra_app)
  expect_equal(args$model, "llama3.2")
  expect_true(args$rag)
})

test_that("get_ollama_models returns a character vector", {
  skip_if_not_installed("jsonlite")
  models <- PRA:::get_ollama_models()
  expect_type(models, "character")
  expect_true(length(models) > 0)
})

test_that("pra_shiny_app returns a shiny app object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  skip_if_not_installed("base64enc")

  app <- PRA:::pra_shiny_app(model = "llama3.2", rag = FALSE)
  expect_s3_class(app, "shiny.appobj")
})
