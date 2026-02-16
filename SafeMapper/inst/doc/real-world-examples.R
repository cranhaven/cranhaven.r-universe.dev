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
# Simulated API function (replace with real API call)
fetch_api_data <- function(id) {
  # Simulate API latency
  Sys.sleep(0.01)
  
  # Simulate occasional failures (5% rate)
  if (runif(1) < 0.05) {
    stop("API Error: Connection timeout")
  }
  
  # Return simulated data
  list(
    id = id,
    value = rnorm(1),
    timestamp = Sys.time()
  )
}

# Configure for API workload
s_configure(
  batch_size = 20,      # Save every 20 calls (~10 seconds of work)
  retry_attempts = 3    # Retry failed calls up to 3 times
)

# Wrap with error handling for graceful failure
safe_fetch <- s_possibly(fetch_api_data, otherwise = NULL)

# Collect data with fault tolerance
ids <- 1:100  # In production: 1:10000

results <- s_map(
  ids,
  safe_fetch,
  .session_id = "api_collection_2026"
)

# Process results
successful <- results[!sapply(results, is.null)]
cat("Successfully collected:", length(successful), "records\n")
cat("Failed:", sum(sapply(results, is.null)), "records\n")

## -----------------------------------------------------------------------------
# Convert successful results to data frame
if (length(successful) > 0) {
  df <- do.call(rbind, lapply(successful, function(x) {
    data.frame(
      id = x$id,
      value = x$value,
      timestamp = as.character(x$timestamp)
    )
  }))
  print(head(df))
}

## -----------------------------------------------------------------------------
# Simulated file processing function
process_file <- function(file_info) {
  # Simulate file processing
  Sys.sleep(0.01)
  
  # Simulate occasional corrupt files
  if (runif(1) < 0.02) {
    stop("Corrupt file: ", file_info$name)
  }
  
  # Return processed result
  list(
    file = file_info$name,
    rows_processed = sample(1000:5000, 1),
    processing_time = runif(1, 20, 40)
  )
}

# Create sample file list
files <- lapply(1:50, function(i) {
  list(
    name = paste0("data_", sprintf("%04d", i), ".csv"),
    path = paste0("/data/raw/data_", sprintf("%04d", i), ".csv"),
    size_mb = runif(1, 10, 100)
  )
})

# Configure for file processing
s_configure(
  batch_size = 10,      # Checkpoint every 10 files
  retry_attempts = 2    # Limited retries (corrupt files won't fix themselves)
)

# Process with error capture
safe_process <- s_safely(process_file)

results <- s_map(
  files,
  safe_process,
  .session_id = "file_batch_2026_01"
)

# Summarize results
successes <- sum(sapply(results, function(x) is.null(x$error)))
failures <- sum(sapply(results, function(x) !is.null(x$error)))

cat("Processed:", successes, "files\n")
cat("Failed:", failures, "files\n")

# Get failure details
failed_files <- sapply(results, function(x) {
  if (!is.null(x$error)) x$error$message else NA
})
failed_files <- failed_files[!is.na(failed_files)]
if (length(failed_files) > 0) {
  cat("\nFailure reasons:\n")
  print(head(failed_files))
}

## -----------------------------------------------------------------------------
# Create parameter grid
param_grid <- expand.grid(
  learning_rate = c(0.01, 0.05, 0.1),
  max_depth = c(3, 5, 7),
  fold = 1:3
)

# Simulated model training function
train_model <- function(params) {
  # Simulate training time
  Sys.sleep(0.01)
  
  # Simulate model performance (depends on hyperparameters)
  base_score <- 0.7
  lr_bonus <- (0.1 - params$learning_rate) * 0.5
  depth_bonus <- params$max_depth * 0.01
  noise <- rnorm(1, 0, 0.05)
  
  list(
    learning_rate = params$learning_rate,
    max_depth = params$max_depth,
    fold = params$fold,
    accuracy = min(1, max(0, base_score + lr_bonus + depth_bonus + noise)),
    training_time = runif(1, 100, 140)
  )
}

# Configure for ML workload
s_configure(
  batch_size = 5,       # Checkpoint every 5 models
  retry_attempts = 2
)

# Convert grid to list for mapping
param_list <- split(param_grid, seq_len(nrow(param_grid)))

# Train all models with checkpointing
results <- s_map(
  param_list,
  train_model,
  .session_id = "cv_grid_search_v1"
)

# Aggregate results
results_df <- do.call(rbind, lapply(results, as.data.frame))

# Find best hyperparameters (average across folds)
best_params <- aggregate(
  accuracy ~ learning_rate + max_depth,
  data = results_df,
  FUN = mean
)
best_params <- best_params[order(-best_params$accuracy), ]
cat("Best hyperparameters:\n")
print(head(best_params, 3))

## -----------------------------------------------------------------------------
# Simulated scraping function
scrape_page <- function(url) {
  # Rate limiting (be respectful!)
  Sys.sleep(0.02)
  
  # Simulate various failure modes
  rand <- runif(1)
  if (rand < 0.03) stop("HTTP 404: Page not found")
  if (rand < 0.05) stop("HTTP 503: Service unavailable")
  if (rand < 0.07) stop("Parsing error: Invalid HTML")
  
  # Return scraped data
  list(
    url = url,
    title = paste("Product", sample(1000:9999, 1)),
    price = round(runif(1, 10, 500), 2),
    rating = round(runif(1, 1, 5), 1),
    scraped_at = Sys.time()
  )
}

# Create URL list
urls <- paste0("https://example.com/product/", 1:100)

# Configure for scraping
s_configure(
  batch_size = 25,      # Checkpoint every 25 pages
  retry_attempts = 3    # Retry for transient errors
)

# Multi-layer error handling
robust_scrape <- s_possibly(scrape_page, otherwise = NULL)

# Scrape with fault tolerance
scraped_data <- s_map(
  urls,
  robust_scrape,
  .session_id = "product_scrape_2026_01"
)

# Analyze results
successful <- scraped_data[!sapply(scraped_data, is.null)]
failed_count <- sum(sapply(scraped_data, is.null))

cat("Successfully scraped:", length(successful), "pages\n")
cat("Failed:", failed_count, "pages\n")

# Convert to data frame
if (length(successful) > 0) {
  products_df <- do.call(rbind, lapply(successful, function(x) {
    data.frame(
      url = x$url,
      title = x$title,
      price = x$price,
      rating = x$rating,
      stringsAsFactors = FALSE
    )
  }))
  
  cat("\nSample products:\n")
  print(head(products_df))
  
  cat("\nPrice statistics:\n")
  print(summary(products_df$price))
}

## ----eval=FALSE---------------------------------------------------------------
# library(future)
# 
# # Set up parallel processing
# plan(multisession, workers = 4)
# 
# # Simulated genomics processing
# process_sequence <- function(seq_info) {
#   # QC
#   Sys.sleep(0.05)
#   qc_pass <- runif(1) > 0.1
# 
#   if (!qc_pass) {
#     return(list(
#       id = seq_info$id,
#       status = "failed_qc",
#       variants = NULL
#     ))
#   }
# 
#   # Alignment + Variant calling + Annotation
#   Sys.sleep(0.1)
# 
#   list(
#     id = seq_info$id,
#     status = "success",
#     variants = sample(0:50, 1),
#     quality_score = runif(1, 20, 40)
#   )
# }
# 
# # Create sequence list
# sequences <- lapply(1:100, function(i) {
#   list(
#     id = paste0("SEQ_", sprintf("%05d", i)),
#     length = sample(1000:5000, 1)
#   )
# })
# 
# # Configure for bioinformatics
# s_configure(
#   batch_size = 20,      # Balance checkpoint frequency and parallel efficiency
#   retry_attempts = 2
# )
# 
# # Process with parallel + fault tolerance
# results <- s_future_map(
#   sequences,
#   process_sequence,
#   .session_id = "genomics_batch_001",
#   .progress = TRUE
# )
# 
# # Clean up parallel backend
# plan(sequential)
# 
# # Summarize
# status_counts <- table(sapply(results, function(x) x$status))
# print(status_counts)
# 
# # Get variant statistics for successful samples
# successful <- results[sapply(results, function(x) x$status == "success")]
# variants <- sapply(successful, function(x) x$variants)
# cat("\nVariant count summary:\n")
# print(summary(variants))

## -----------------------------------------------------------------------------
# Simulated migration function
migrate_record <- function(record) {
  # Simulate read from source
  Sys.sleep(0.005)
  
  # Simulate occasional failures
  if (runif(1) < 0.02) {
    stop("Database connection error")
  }
  
  # Transform
  transformed <- list(
    id = record$id,
    new_field = paste(record$name, record$category, sep = "_"),
    migrated_at = Sys.time(),
    source_hash = digest::digest(record)
  )
  
  # Simulate write to target
  Sys.sleep(0.005)
  
  list(
    original_id = record$id,
    new_id = transformed$id,
    status = "migrated"
  )
}

# Create sample records
records <- lapply(1:200, function(i) {
  list(
    id = i,
    name = paste0("Record_", i),
    category = sample(c("A", "B", "C"), 1),
    value = rnorm(1)
  )
})

# Configure for database operations
s_configure(
  batch_size = 50,      # Reasonable transaction size
  retry_attempts = 5    # Database errors often transient
)

# Migrate with fault tolerance
safe_migrate <- s_safely(migrate_record)

migration_results <- s_map(
  records,
  safe_migrate,
  .session_id = "db_migration_v1"
)

# Generate migration report
successful <- sum(sapply(migration_results, function(x) is.null(x$error)))
failed <- sum(sapply(migration_results, function(x) !is.null(x$error)))

cat("Migration Report\n")
cat("================\n")
cat("Total records:", length(records), "\n")
cat("Migrated:", successful, "\n")
cat("Failed:", failed, "\n")
cat("Success rate:", round(successful / length(records) * 100, 2), "%\n")

# Export failed record IDs for investigation
failed_ids <- sapply(seq_along(migration_results), function(i) {
  if (!is.null(migration_results[[i]]$error)) records[[i]]$id else NA
})
failed_ids <- failed_ids[!is.na(failed_ids)]
if (length(failed_ids) > 0) {
  cat("\nFailed record IDs:", paste(head(failed_ids, 10), collapse = ", "))
  if (length(failed_ids) > 10) cat("...")
  cat("\n")
}

