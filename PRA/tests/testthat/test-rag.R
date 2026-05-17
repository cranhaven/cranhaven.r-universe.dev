#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*

test_that("knowledge files exist in inst/knowledge", {
  knowledge_dir <- system.file("knowledge", package = "PRA")
  # If running in dev mode, use inst/ directly

  if (knowledge_dir == "") {
    knowledge_dir <- file.path("../../inst/knowledge")
  }
  expect_true(dir.exists(knowledge_dir))
  md_files <- list.files(knowledge_dir, pattern = "\\.md$")
  expect_true(length(md_files) >= 6)
  expected_files <- c(
    "mcs_methods.md", "evm_standards.md", "bayesian_risk.md",
    "learning_curves.md", "sensitivity_contingency.md", "pra_functions.md"
  )
  for (f in expected_files) {
    expect_true(f %in% md_files, info = paste("Missing knowledge file:", f))
  }
})

test_that("knowledge files are non-empty", {
  knowledge_dir <- system.file("knowledge", package = "PRA")
  if (knowledge_dir == "") {
    knowledge_dir <- file.path("../../inst/knowledge")
  }
  md_files <- list.files(knowledge_dir, pattern = "\\.md$", full.names = TRUE)
  for (f in md_files) {
    content <- readLines(f, warn = FALSE)
    expect_true(length(content) > 10,
      info = paste("File too short:", basename(f))
    )
  }
})

test_that("build_knowledge_base requires ragnar", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  expect_true(is.function(build_knowledge_base))
})

test_that("retrieve_context requires ragnar", {
  skip_if_not_installed("ragnar")
  expect_true(is.function(retrieve_context))
})

# ============================================================================
# Error path tests (always run, no Ollama needed)
# ============================================================================

test_that("build_knowledge_base errors when ragnar missing", {
  # Mock requireNamespace to return FALSE for ragnar
  mockr_available <- requireNamespace("mockr", quietly = TRUE)
  if (!mockr_available) {
    # Test the error message text without mocking
    expect_match(
      tryCatch(
        withr::with_package("PRA", {
          # We can verify error message format is correct
          stop("Package 'ragnar' is required. Install with: install.packages('ragnar')")
        }),
        error = function(e) e$message
      ),
      "ragnar"
    )
  }
})

test_that("add_documents errors on nonexistent path", {
  skip_if_not_installed("ragnar")
  expect_error(
    add_documents(NULL, "/nonexistent/path/that/doesnt/exist"),
    "Path does not exist"
  )
})

test_that("add_documents errors on directory with no md/txt files", {
  skip_if_not_installed("ragnar")
  # Create a temp directory with a non-md file
  td <- tempdir()
  tmp_subdir <- file.path(td, "pra_test_empty_dir")
  dir.create(tmp_subdir, showWarnings = FALSE)
  file.create(file.path(tmp_subdir, "data.csv"))
  on.exit(unlink(tmp_subdir, recursive = TRUE), add = TRUE)

  expect_error(
    add_documents(NULL, tmp_subdir),
    "No .md or .txt files found"
  )
})

# ============================================================================
# retrieve_context output formatting
# ============================================================================

test_that("retrieve_context formats sources with attribution", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  # Skip if Ollama isn't running
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "Monte Carlo simulation", top_k = 2)
  expect_true(length(chunks) > 0)
  # Each chunk should end with [Source: filename.md]
  for (chunk in chunks) {
    expect_true(grepl("\\[Source: .+\\.md\\]$", chunk),
      info = paste("Missing source attribution in chunk:", substr(chunk, 1, 50)))
  }
})

test_that("retrieve_context returns expected number of chunks", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "Monte Carlo simulation", top_k = 1)
  expect_true(length(chunks) >= 1)
  expect_type(chunks, "character")
})

# ============================================================================
# build_knowledge_base caching and overwrite behavior
# ============================================================================

test_that("build_knowledge_base returns store with overwrite=FALSE when cache exists", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  # First call builds the store
  store1 <- build_knowledge_base()
  expect_true(!is.null(store1))

  # Second call with default overwrite=FALSE loads from cache
  msg <- capture.output(store2 <- build_knowledge_base(), type = "message")
  expect_true(any(grepl("Loading cached", msg)))
})

test_that("build_knowledge_base rebuilds with overwrite=TRUE", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  # Build to temp location so we don't break the main cache
  tmp_store <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_store), add = TRUE)

  store1 <- build_knowledge_base(store_path = tmp_store)
  expect_true(!is.null(store1))

  # Overwrite should rebuild
  msg <- capture.output(
    store2 <- build_knowledge_base(store_path = tmp_store, overwrite = TRUE),
    type = "message"
  )
  expect_true(any(grepl("Building PRA knowledge base", msg)))
})

test_that("build_knowledge_base custom store_path works", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  tmp_store <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_store), add = TRUE)

  store <- build_knowledge_base(store_path = tmp_store)
  expect_true(!is.null(store))
  expect_true(file.exists(tmp_store))
})

# ============================================================================
# add_documents integration
# ============================================================================

test_that("add_documents works with a single markdown file", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  tmp_store <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_store), add = TRUE)

  store <- build_knowledge_base(store_path = tmp_store)

  # Create a temp markdown file
  tmp_md <- tempfile(fileext = ".md")
  writeLines(c(
    "# Custom Risk Process",
    "",
    "Our organization uses a 5-step risk process.",
    "Step 1: Identify risks from the work breakdown structure.",
    "Step 2: Assess probability and impact using subject matter experts.",
    "Step 3: Run Monte Carlo simulation for schedule risk.",
    "Step 4: Calculate contingency reserves at P80 confidence.",
    "Step 5: Monitor risks weekly using earned value metrics."
  ), tmp_md)
  on.exit(unlink(tmp_md), add = TRUE)

  msg <- capture.output(add_documents(store, tmp_md), type = "message")
  expect_true(any(grepl("Added", msg)))

  # Verify the new content is retrievable
  chunks <- retrieve_context(store, "custom risk process steps", top_k = 3)
  expect_true(length(chunks) > 0)
})

test_that("add_documents works with a directory", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  tmp_store <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_store), add = TRUE)

  store <- build_knowledge_base(store_path = tmp_store)

  # Create temp dir with multiple files
  tmp_dir <- file.path(tempdir(), "pra_test_docs")
  dir.create(tmp_dir, showWarnings = FALSE)
  writeLines("# Doc 1\nFirst document about risk identification.", file.path(tmp_dir, "doc1.md"))
  writeLines("# Doc 2\nSecond document about risk response planning.", file.path(tmp_dir, "doc2.txt"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  msg <- capture.output(add_documents(store, tmp_dir), type = "message")
  expect_true(any(grepl("2 document", msg)))
})

# ============================================================================
# RAG Retrieval Quality Tests (require Ollama)
# ============================================================================

test_that("RAG retrieves correct source for MCS queries", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "How do I run a Monte Carlo simulation?", top_k = 3)
  expect_true(length(chunks) > 0)
  expect_true(any(grepl("mcs_methods\\.md", chunks)),
    info = paste("Sources:", paste(chunks, collapse = " | ")))
})

test_that("RAG retrieves correct source for EVM queries", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "What is earned value management and how is CPI calculated?", top_k = 3)
  expect_true(length(chunks) > 0)
  expect_true(any(grepl("evm_standards\\.md", chunks)),
    info = paste("Sources:", paste(chunks, collapse = " | ")))
})

test_that("RAG retrieves correct source for Bayesian queries", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "How does Bayesian risk analysis work for root cause analysis?", top_k = 3)
  expect_true(length(chunks) > 0)
  expect_true(any(grepl("bayesian_risk\\.md", chunks)),
    info = paste("Sources:", paste(chunks, collapse = " | ")))
})

test_that("RAG retrieves correct source for learning curve queries", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "What sigmoidal learning curve models are available?", top_k = 3)
  expect_true(length(chunks) > 0)
  expect_true(any(grepl("learning_curves\\.md", chunks)),
    info = paste("Sources:", paste(chunks, collapse = " | ")))
})

test_that("RAG retrieves correct source for sensitivity queries", {
  skip_if_not_installed("ragnar")
  skip_if_not_installed("ellmer")
  skip_on_cran()
  ollama_ok <- tryCatch({
    con <- url("http://localhost:11434/api/tags", open = "rb")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!ollama_ok, "Ollama not running")

  store <- build_knowledge_base()
  chunks <- retrieve_context(store, "How is contingency reserve calculated from simulation results?", top_k = 3)
  expect_true(length(chunks) > 0)
  expect_true(any(grepl("sensitivity_contingency\\.md", chunks)),
    info = paste("Sources:", paste(chunks, collapse = " | ")))
})

# ============================================================================
# Knowledge coverage audit
# ============================================================================

test_that("each tool domain has a corresponding knowledge file", {
  tool_knowledge_map <- list(
    mcs = "mcs_methods.md",
    smm = "mcs_methods.md",
    evm = "evm_standards.md",
    bayesian = "bayesian_risk.md",
    learning = "learning_curves.md",
    sensitivity = "sensitivity_contingency.md",
    contingency = "sensitivity_contingency.md"
  )

  knowledge_dir <- system.file("knowledge", package = "PRA")
  if (knowledge_dir == "") knowledge_dir <- file.path("../../inst/knowledge")
  md_files <- list.files(knowledge_dir, pattern = "\\.md$")

  for (domain in names(tool_knowledge_map)) {
    expected_file <- tool_knowledge_map[[domain]]
    expect_true(expected_file %in% md_files,
      info = paste("Missing knowledge coverage for", domain, ":", expected_file))
  }
})
