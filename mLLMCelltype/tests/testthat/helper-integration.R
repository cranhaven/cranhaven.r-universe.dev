skip_if_not_cran_env <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    testthat::skip("Set NOT_CRAN=true to run this test")
  }
}

skip_if_not_integration <- function() {
  enabled <- tolower(Sys.getenv("MLLMCELLTYPE_RUN_INTEGRATION"))
  if (!enabled %in% c("1", "true", "yes", "on")) {
    testthat::skip("Set MLLMCELLTYPE_RUN_INTEGRATION=true to run real API integration tests")
  }
}

skip_if_no_openrouter_key <- function() {
  if (!nzchar(Sys.getenv("OPENROUTER_API_KEY"))) {
    testthat::skip("No OpenRouter API key available")
  }
}

skip_real_api_test <- function() {
  skip_if_not_cran_env()
  skip_if_not_integration()
  skip_if_no_openrouter_key()
}

openrouter_test_models <- c("meta-llama/llama-4-maverick:free", "meta-llama/llama-3.3-70b-instruct:free")

openrouter_api_keys <- function() {
  list(openrouter = Sys.getenv("OPENROUTER_API_KEY"))
}

minimal_pbmc_markers <- function() {
  data.frame(
    cluster = c(0, 0, 0, 1, 1, 1),
    gene = c("CD3D", "CD8A", "IL7R", "CD14", "LYZ", "S100A8"),
    avg_log2FC = c(3.2, 2.1, 1.2, 2.8, 2.3, 1.9)
  )
}
