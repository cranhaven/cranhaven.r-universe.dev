## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(SafeMapper)

## -----------------------------------------------------------------------------
# Configure retry behavior
s_configure(
  retry_attempts = 5,  # Try up to 5 times per batch
  batch_size = 50      # Each batch contains 50 items
)

## -----------------------------------------------------------------------------
# Create a safe version of log
safe_log <- s_safely(log)

# Successful call
result <- safe_log(10)
print(result)

# Error call (returns error instead of throwing)
result <- safe_log("not a number")
print(result)

## -----------------------------------------------------------------------------
# Define function that might fail
risky_operation <- function(x) {
  if (x < 0) stop("Negative values not allowed")
  sqrt(x)
}

# Wrap with s_safely
safe_operation <- s_safely(risky_operation)

# Apply to data that includes problematic values
data <- c(4, -1, 9, -4, 16)
results <- s_map(data, safe_operation)

# Extract successful results
successes <- s_map_dbl(results, ~ .x$result %||% NA_real_)
print(successes)

# Check which failed
errors <- s_map_lgl(results, ~ !is.null(.x$error))
print(errors)

## -----------------------------------------------------------------------------
# Create a function that returns NA on error
possible_log <- s_possibly(log, otherwise = NA_real_)

# Mix of valid and invalid inputs
inputs <- list(10, "text", 100, NULL, 1000)
results <- s_map_dbl(inputs, possible_log)
print(results)

## -----------------------------------------------------------------------------
# Simulated data extraction that might fail
extract_value <- function(x) {
  if (is.null(x) || length(x) == 0) stop("Invalid input")
  x[[1]]
}

# Wrap with default value
safe_extract <- s_possibly(extract_value, otherwise = NA)

# Apply to mixed data
data <- list(
  list(value = 1),
  NULL,
  list(value = 3),
  list(),
  list(value = 5)
)

results <- s_map(data, safe_extract)
print(unlist(results))

## -----------------------------------------------------------------------------
# Function with side effects
chatty_function <- function(x) {
  message("Processing: ", x)
  if (x > 5) warning("Large value!")
  cat("Result is:", x^2, "\n")
  x^2
}

# Wrap with s_quietly
quiet_function <- s_quietly(chatty_function)

# Call it - no output during execution
result <- quiet_function(7)

# Examine captured side effects
print(names(result))
print(result$result)
print(result$messages)
print(result$warnings)
print(result$output)

## -----------------------------------------------------------------------------
# Best for: When you need to analyze failures

process_item <- function(x) {
  if (runif(1) < 0.3) stop("Random failure")
  x^2
}

safe_process <- s_safely(process_item)

# Process with checkpointing + error capture
results <- s_map(1:10, safe_process)

# Analyze results
success_count <- sum(s_map_lgl(results, ~ is.null(.x$error)))
failure_count <- sum(s_map_lgl(results, ~ !is.null(.x$error)))

cat("Successes:", success_count, "\n")
cat("Failures:", failure_count, "\n")

# Get successful values
successful_values <- s_map_dbl(results, function(r) {
  if (is.null(r$error)) r$result else NA_real_
})
print(successful_values)

## -----------------------------------------------------------------------------
# Best for: When you just need results, failures = NA

robust_sqrt <- s_possibly(
  function(x) {
    if (x < 0) stop("negative")
    sqrt(x)
  },
  otherwise = NA_real_
)

# Clean pipeline
data <- c(4, -1, 9, -4, 16, -9, 25)
results <- s_map_dbl(data, robust_sqrt)
print(results)

# Easy to filter out failures
valid_results <- results[!is.na(results)]
print(valid_results)

## -----------------------------------------------------------------------------
# Best for: Critical operations that must not fail

# Layer 1: Function-level error handling
robust_api_call <- function(x) {
  tryCatch({
    # Simulate API call that might fail
    if (runif(1) < 0.2) stop("Temporary failure")
    x * 10
  }, error = function(e) {
    NA_real_  # Return NA on error
  })
}

# Layer 2: s_possibly for unexpected errors
safe_api_call <- s_possibly(robust_api_call, otherwise = NA_real_)

# Layer 3: SafeMapper checkpointing
results <- s_map_dbl(
  1:20, 
  safe_api_call,
  .session_id = "critical_operation"
)

print(results)
cat("Success rate:", mean(!is.na(results)) * 100, "%\n")

## ----eval=FALSE---------------------------------------------------------------
# # No error wrapping - errors will stop execution
# # Previous batches are saved to checkpoint
# results <- s_map(data, risky_function)

## -----------------------------------------------------------------------------
# Create a logging wrapper
log_errors <- function(f) {
  function(...) {
    tryCatch(
      f(...),
      error = function(e) {
        message("Error: ", e$message)
        NA
      }
    )
  }
}

# Use with s_map
logged_sqrt <- log_errors(function(x) {
  if (x < 0) stop("negative input")
  sqrt(x)
})

results <- s_map_dbl(c(4, -1, 9, -4, 16), logged_sqrt)
print(results)

## -----------------------------------------------------------------------------
# Process and collect all errors
process_with_tracking <- function(items) {
  safe_fn <- s_safely(function(x) {
    if (x %% 3 == 0) stop("Divisible by 3")
    x^2
  })
  
  results <- s_map(items, safe_fn)
  
  # Build report
  list(
    values = s_map(results, "result"),
    errors = s_map(results, "error"),
    success_rate = mean(s_map_lgl(results, ~ is.null(.x$error)))
  )
}

report <- process_with_tracking(1:10)
cat("Success rate:", report$success_rate * 100, "%\n")

