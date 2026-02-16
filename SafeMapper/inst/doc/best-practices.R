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
# Production pipeline template
run_production_pipeline <- function(data, process_fn, config = list()) {
  # ===== Configuration Layer =====
  default_config <- list(
    batch_size = 100,
    retry_attempts = 3,
    session_id = paste0("pipeline_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    fail_threshold = 0.1  # Max 10% failure rate
  )
  config <- modifyList(default_config, config)
  
  s_configure(
    batch_size = config$batch_size,
    retry_attempts = config$retry_attempts
  )
  
  # ===== Input Validation =====
  if (length(data) == 0) {
    stop("No data provided")
  }
  
  message(sprintf("[%s] Starting pipeline: %d items", Sys.time(), length(data)))
  
  # ===== SafeMapper Processing =====
  safe_fn <- s_safely(process_fn)
  
  results <- s_map(
    data,
    safe_fn,
    .session_id = config$session_id
  )
  
  # ===== Post-Processing =====
  successes <- sum(sapply(results, function(x) is.null(x$error)))
  failures <- sum(sapply(results, function(x) !is.null(x$error)))
  success_rate <- successes / length(results)
  
  message(sprintf("[%s] Complete: %d/%d (%.1f%% success)", 
                  Sys.time(), successes, length(results), success_rate * 100))
  
  # Check failure threshold
  if ((1 - success_rate) > config$fail_threshold) {
    warning(sprintf("Failure rate (%.1f%%) exceeds threshold (%.1f%%)",
                    (1 - success_rate) * 100, config$fail_threshold * 100))
  }
  
  # Return structured result
  list(
    results = results,
    summary = list(
      total = length(results),
      successes = successes,
      failures = failures,
      success_rate = success_rate
    ),
    config = config
  )
}

# Usage example
sample_data <- 1:50
output <- run_production_pipeline(
  sample_data,
  function(x) x^2,
  config = list(batch_size = 10, session_id = "demo_pipeline")
)
print(output$summary)

## -----------------------------------------------------------------------------
# config.R - Environment-specific settings

get_config <- function(env = Sys.getenv("R_ENV", "development")) {
  configs <- list(
    development = list(
      batch_size = 10,
      retry_attempts = 1,
      auto_recover = FALSE,
      session_prefix = "dev"
    ),
    staging = list(
      batch_size = 50,
      retry_attempts = 3,
      auto_recover = TRUE,
      session_prefix = "staging"
    ),
    production = list(
      batch_size = 100,
      retry_attempts = 5,
      auto_recover = TRUE,
      session_prefix = "prod"
    )
  )
  
  config <- configs[[env]]
  if (is.null(config)) {
    warning("Unknown environment, using development settings")
    config <- configs$development
  }
  
  config
}

# Usage
config <- get_config("production")
s_configure(
  batch_size = config$batch_size,
  retry_attempts = config$retry_attempts,
  auto_recover = config$auto_recover
)

## -----------------------------------------------------------------------------
# Layer 1: Function-level error handling
process_with_validation <- function(item) {
  # Input validation
  if (is.null(item) || length(item) == 0) {
    return(list(status = "skipped", reason = "empty input"))
  }
  
  # Try the operation
  tryCatch({
    result <- item^2  # Your actual operation
    list(status = "success", value = result)
  }, error = function(e) {
    list(status = "error", reason = e$message)
  })
}

# Layer 2: s_possibly for unexpected errors
safe_process <- s_possibly(process_with_validation, 
                           otherwise = list(status = "failed", reason = "unknown"))

# Layer 3: SafeMapper checkpointing
results <- s_map(
  list(1, NULL, 3, "invalid", 5),
  safe_process,
  .session_id = "three_layer_demo"
)

# Review results
statuses <- sapply(results, function(x) x$status)
table(statuses)

## -----------------------------------------------------------------------------
# Recommended session ID formats

# Time-based (for scheduled jobs)
session_daily <- paste0("daily_report_", format(Sys.Date(), "%Y%m%d"))

# Version-based (for algorithm changes)
session_versioned <- "model_training_v2.1"

# Task-based (for specific operations)
session_task <- "customer_data_migration_batch1"

# Combined (recommended for production)
session_production <- paste0(
  "prod_",                                    # Environment
  "data_sync_",                               # Task name
  format(Sys.time(), "%Y%m%d_%H%M%S"),       # Timestamp
  "_v1"                                       # Version
)

print(session_production)

## -----------------------------------------------------------------------------
# cleanup_jobs.R - Run periodically

perform_cleanup <- function() {
  message("Starting checkpoint cleanup...")
  
  # Remove old sessions (> 7 days)
  removed <- s_clean_sessions(older_than_days = 7)
  message(sprintf("Removed %d old session files", removed))
  
  # Note: In a real scenario, you might want to:
  # - Archive before deletion
  # - Send notifications for failed sessions
  # - Log cleanup activities
  
  invisible(removed)
}

# Run cleanup
perform_cleanup()

## -----------------------------------------------------------------------------
# Performance comparison helper
benchmark_batch_sizes <- function(data, func, sizes = c(10, 50, 100, 200)) {
  results <- list()
  
  for (size in sizes) {
    s_configure(batch_size = size)
    
    start_time <- Sys.time()
    s_map(data, func, .session_id = paste0("bench_", size))
    end_time <- Sys.time()
    
    results[[as.character(size)]] <- as.numeric(end_time - start_time)
    
    # Clean up benchmark sessions
    s_clean_sessions(session_ids = paste0("bench_", size))
  }
  
  data.frame(
    batch_size = sizes,
    time_seconds = unlist(results)
  )
}

# Example (with small data for demo)
# In production, use larger datasets
test_data <- 1:100
results <- benchmark_batch_sizes(
  test_data, 
  function(x) { Sys.sleep(0.001); x^2 },
  sizes = c(10, 25, 50)
)
print(results)

## -----------------------------------------------------------------------------
# Custom progress wrapper
process_with_logging <- function(items, func, session_id) {
  total <- length(items)
  start_time <- Sys.time()
  
  message(sprintf("[START] %s - Processing %d items", session_id, total))
  
  results <- s_map(items, function(item) {
    result <- func(item)
    result
  }, .session_id = session_id)
  
  end_time <- Sys.time()
  duration <- as.numeric(end_time - start_time, units = "secs")
  
  message(sprintf("[END] %s - Completed in %.2f seconds (%.2f items/sec)", 
                  session_id, duration, total / duration))
  
  results
}

# Usage
results <- process_with_logging(
  1:50,
  function(x) { Sys.sleep(0.01); x^2 },
  "logged_task"
)

## -----------------------------------------------------------------------------
# Quick reference for production use

# 1. Configure appropriately
s_configure(
  batch_size = 100,     # Tune based on operation speed
  retry_attempts = 3    # Higher for network operations
)

# 2. Use descriptive session IDs
session_id <- paste0("prod_task_", format(Sys.time(), "%Y%m%d"))

# 3. Wrap functions for safety (using s_safely for error capture)
my_function <- function(x) x^2
safe_fn <- s_safely(my_function)

# 4. Process with fault tolerance
data <- 1:20
results <- s_map(data, safe_fn, .session_id = session_id)

# 5. Handle results properly (s_safely returns list with result/error)
successes <- sum(sapply(results, function(x) is.null(x$error)))
cat("Success rate:", successes / length(results) * 100, "%\n")

# 6. Clean up periodically
s_clean_sessions(older_than_days = 7)

