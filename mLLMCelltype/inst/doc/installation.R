## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # Install from CRAN
# install.packages("mLLMCelltype")

## -----------------------------------------------------------------------------
# # Install devtools if not already installed
# if (!requireNamespace("devtools", quietly = TRUE)) {
#   install.packages("devtools")
# }
# 
# # Install mLLMCelltype development version
# devtools::install_github("cafferychen777/mLLMCelltype", subdir = "R")

## -----------------------------------------------------------------------------
# # Assuming the package is in the current working directory
# devtools::install_local("path/to/mLLMCelltype/R")

## -----------------------------------------------------------------------------
# library(dotenv)
# dotenv::load_dot_env()

## -----------------------------------------------------------------------------
# library(mLLMCelltype)
# 
# results <- annotate_cell_types(
#   input = your_marker_data,
#   tissue_name = "human PBMC",
#   model = "claude-sonnet-4-6",
#   api_key = "your-anthropic-key",
#   top_gene_count = 10
# )

## -----------------------------------------------------------------------------
# Sys.setenv(OPENAI_API_KEY = "your-openai-key")
# Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-key")
# # Set other API keys as needed

## -----------------------------------------------------------------------------
# library(mLLMCelltype)
# 
# # Check if the package is loaded correctly
# packageVersion("mLLMCelltype")
# 
# # Verify API key setup for a specific provider
# api_key <- get_api_key("anthropic")
# if (!is.null(api_key) && api_key != "") {
#   cat("Anthropic API key is set up correctly\n")
# } else {
#   cat("Anthropic API key is not set up\n")
# }

## -----------------------------------------------------------------------------
# # Example of setting proxy for httr
# httr::set_config(httr::use_proxy(url = "proxy_url", port = proxy_port))

