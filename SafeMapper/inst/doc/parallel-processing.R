## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(SafeMapper)

## ----eval=FALSE---------------------------------------------------------------
# install.packages(c("furrr", "future"))

## ----eval=FALSE---------------------------------------------------------------
# library(SafeMapper)
# library(future)

## ----eval=FALSE---------------------------------------------------------------
# # Use multiple R sessions (works on all platforms)
# plan(multisession, workers = 4)
# 
# # Or use forked processes (faster, but Unix/Mac only)
# # plan(multicore, workers = 4)

## ----eval=FALSE---------------------------------------------------------------
# # Instead of furrr::future_map()
# result <- s_future_map(1:1000, expensive_function)

## ----eval=FALSE---------------------------------------------------------------
# library(future)
# plan(multisession, workers = 2)
# 
# # CPU-intensive computation
# result <- s_future_map(1:100, function(x) {
#   Sys.sleep(0.1)  # Simulate work
#   x^2
# })
# 
# # Reset to sequential
# plan(sequential)

## ----eval=FALSE---------------------------------------------------------------
# plan(multisession, workers = 2)
# 
# x <- 1:50
# y <- 51:100
# 
# # Process pairs in parallel
# results <- s_future_map2(x, y, function(a, b) {
#   Sys.sleep(0.1)
#   a * b
# })
# 
# plan(sequential)

## ----eval=FALSE---------------------------------------------------------------
# plan(multisession, workers = 2)
# 
# params <- list(
#   a = 1:30,
#   b = 31:60,
#   c = 61:90
# )
# 
# # Process multiple inputs in parallel
# results <- s_future_pmap(params, function(a, b, c) {
#   Sys.sleep(0.1)
#   a + b + c
# })
# 
# plan(sequential)

## -----------------------------------------------------------------------------
# Larger batches = more efficient parallel execution
# But less frequent checkpoints
s_configure(batch_size = 200)

# Smaller batches = more frequent checkpoints  
# But more overhead from parallelization
s_configure(batch_size = 50)

## ----eval=FALSE---------------------------------------------------------------
# # Custom furrr options
# opts <- furrr::furrr_options(
#   seed = 123,           # Reproducible random numbers
#   globals = TRUE,       # Export global variables
#   packages = "dplyr"    # Load packages in workers
# )
# 
# result <- s_future_map(
#   1:100,
#   my_function,
#   .options = opts
# )

## ----eval=FALSE---------------------------------------------------------------
# # 1. Heavy computations
# s_future_map(large_datasets, function(data) {
#   # Complex statistical model fitting
#   fit_complex_model(data)
# })
# 
# # 2. Image/file processing
# s_future_map(image_files, function(file) {
#   # CPU-intensive image transformation
#   process_image(file)
# })
# 
# # 3. Simulations
# s_future_map(1:1000, function(i) {
#   # Monte Carlo simulation
#   run_simulation(seed = i)
# })

## ----eval=FALSE---------------------------------------------------------------
# # 1. Simple operations (overhead > benefit)
# # DON'T:
# s_future_map(1:1000, ~ .x + 1)  # Too simple
# # DO:
# s_map(1:1000, ~ .x + 1)
# 
# # 2. Rate-limited API calls
# # DON'T:
# s_future_map(urls, fetch_api)  # May hit rate limits
# # DO:
# s_map(urls, fetch_api)  # Sequential respects rate limits

## ----eval=FALSE---------------------------------------------------------------
# # Enable progress bar
# result <- s_future_map(
#   1:100,
#   slow_function,
#   .progress = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# library(SafeMapper)
# library(future)
# 
# # Configure parallel backend
# plan(multisession, workers = 4)
# 
# # Configure SafeMapper
# s_configure(
#   batch_size = 100,     # Checkpoint every 100 items
#   retry_attempts = 3    # Retry failed batches 3 times
# )
# 
# # Define your processing function
# process_item <- function(x) {
#   Sys.sleep(0.5)  # Simulate work
#   result <- x^2 + rnorm(1)
#   return(result)
# }
# 
# # Run with fault tolerance
# results <- s_future_map(
#   1:500,
#   process_item,
#   .progress = TRUE,
#   .options = furrr::furrr_options(seed = 42)
# )
# 
# # Clean up
# plan(sequential)
# 
# # If interrupted, just re-run the same code!
# # It will resume from the last checkpoint.

