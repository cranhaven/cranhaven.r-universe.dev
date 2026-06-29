# =============================================================================
# API Rate Limit Testing Script
# =============================================================================
# Purpose: Test different parallelization strategies against API rate limits
# Author: Performance Analysis Team
# Date: 2025-11-14
# =============================================================================

library(mLLMCelltype)

# Load environment variables
if (file.exists(".env")) {
  readRenviron(".env")
}

# =============================================================================
# Test Configuration
# =============================================================================

# Test data: Small dataset for quick iteration
test_clusters <- list(
  "0" = list(genes = c("CD3D", "CD3E", "CD3G", "CD28", "TRAC", "TRBC1", "IL7R", "LCK", "CD2", "CD7")),
  "1" = list(genes = c("CD79A", "CD79B", "MS4A1", "CD19", "BANK1", "PAX5", "BLK", "CD22", "TCL1A", "IGHM")),
  "2" = list(genes = c("CD14", "CD68", "CSF1R", "FCGR3A", "LYZ", "S100A8", "S100A9", "VCAN", "FCN1", "MNDA"))
)

tissue_name <- "human PBMC"

# Models to test
models <- c(
  "claude-sonnet-4-6",
  "claude-haiku-4-5-20251001"
)

# API keys
api_keys <- list(
  anthropic = Sys.getenv("ANTHROPIC_API_KEY")
)

# Verify API key
if (api_keys$anthropic == "") {
  stop("ANTHROPIC_API_KEY not found in environment. Please set it in .env file.")
}

cat("=============================================================================\n")
cat("API RATE LIMIT TESTING\n")
cat("=============================================================================\n")
cat(sprintf("Test dataset: %d clusters\n", length(test_clusters)))
cat(sprintf("Models to test: %d (%s)\n", length(models), paste(models, collapse = ", ")))
cat(sprintf("API Key found: %s...\n", substr(api_keys$anthropic, 1, 20)))
cat("=============================================================================\n\n")

# =============================================================================
# Test 1: Serial Execution (Baseline)
# =============================================================================

test_serial <- function() {
  cat("\n", strrep("=", 80), "\n")
  cat("TEST 1: SERIAL EXECUTION (BASELINE)\n")
  cat(strrep("=", 80), "\n\n")

  start_time <- Sys.time()
  results <- list()
  api_errors <- list()

  for (i in seq_along(models)) {
    model <- models[i]
    cat(sprintf("[%d/%d] Calling %s...\n", i, length(models), model))

    model_start <- Sys.time()

    result <- tryCatch({
      annotate_cell_types(
        input = test_clusters,
        tissue_name = tissue_name,
        model = model,
        api_key = api_keys$anthropic,
        top_gene_count = 10
      )
    }, error = function(e) {
      list(error = TRUE, message = e$message)
    })

    model_duration <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

    if (is.list(result) && !is.null(result$error)) {
      cat(sprintf("  ❌ ERROR: %s\n", result$message))
      api_errors[[model]] <- result$message
    } else {
      cat(sprintf("  ✓ Success (%.2f seconds)\n", model_duration))
      results[[model]] <- result
    }

    # Small delay between requests
    Sys.sleep(0.5)
  }

  total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat("\n", strrep("-", 80), "\n")
  cat(sprintf("Total Duration: %.2f seconds\n", total_duration))
  cat(sprintf("Success Rate: %d/%d (%.0f%%)\n",
              length(results), length(models),
              100 * length(results) / length(models)))
  cat(sprintf("Average per model: %.2f seconds\n", total_duration / length(models)))

  if (length(api_errors) > 0) {
    cat("\nErrors encountered:\n")
    for (model in names(api_errors)) {
      cat(sprintf("  - %s: %s\n", model, api_errors[[model]]))
    }
  }

  return(list(
    duration = total_duration,
    results = results,
    errors = api_errors,
    success_rate = length(results) / length(models)
  ))
}

# =============================================================================
# Test 2: Parallel Execution (No Rate Limiting)
# =============================================================================

test_parallel_no_limit <- function() {
  cat("\n", strrep("=", 80), "\n")
  cat("TEST 2: PARALLEL EXECUTION (NO RATE LIMITING)\n")
  cat(strrep("=", 80), "\n\n")

  if (!requireNamespace("future", quietly = TRUE)) {
    cat("⚠️  'future' package not installed. Skipping test.\n")
    cat("Install with: install.packages('future')\n")
    return(NULL)
  }

  library(future)
  library(future.apply)

  # Setup parallel backend
  plan(multisession, workers = length(models))

  cat(sprintf("Parallel workers: %d\n", length(models)))
  cat("Launching all API calls simultaneously...\n\n")

  start_time <- Sys.time()

  results_list <- future_lapply(seq_along(models), function(i) {
    model <- models[i]

    model_start <- Sys.time()

    result <- tryCatch({
      annotate_cell_types(
        input = test_clusters,
        tissue_name = tissue_name,
        model = model,
        api_key = api_keys$anthropic,
        top_gene_count = 10
      )
    }, error = function(e) {
      list(error = TRUE, message = e$message, model = model)
    })

    model_duration <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

    return(list(
      model = model,
      result = result,
      duration = model_duration,
      error = !is.null(result$error) && result$error
    ))
  }, future.seed = TRUE)

  total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Reset to sequential
  plan(sequential)

  # Analyze results
  results <- list()
  api_errors <- list()

  for (item in results_list) {
    cat(sprintf("[%s] ", item$model))
    if (item$error) {
      cat(sprintf("❌ ERROR: %s\n", item$result$message))
      api_errors[[item$model]] <- item$result$message
    } else {
      cat(sprintf("✓ Success (%.2f seconds)\n", item$duration))
      results[[item$model]] <- item$result
    }
  }

  cat("\n", strrep("-", 80), "\n")
  cat(sprintf("Total Duration: %.2f seconds\n", total_duration))
  cat(sprintf("Success Rate: %d/%d (%.0f%%)\n",
              length(results), length(models),
              100 * length(results) / length(models)))

  if (length(results) > 0) {
    avg_model_duration <- mean(sapply(results_list[!sapply(results_list, function(x) x$error)],
                                       function(x) x$duration))
    cat(sprintf("Average per model: %.2f seconds\n", avg_model_duration))
    cat(sprintf("Speedup vs Serial: %.2fx\n",
                (length(models) * avg_model_duration) / total_duration))
  }

  if (length(api_errors) > 0) {
    cat("\n⚠️  ERRORS ENCOUNTERED:\n")
    for (model in names(api_errors)) {
      cat(sprintf("  - %s: %s\n", model, api_errors[[model]]))
    }
  }

  return(list(
    duration = total_duration,
    results = results,
    errors = api_errors,
    success_rate = length(results) / length(models)
  ))
}

# =============================================================================
# Test 3: Parallel with Rate Limiting (Throttled)
# =============================================================================

test_parallel_with_throttle <- function(delay_seconds = 1.0) {
  cat("\n", strrep("=", 80), "\n")
  cat(sprintf("TEST 3: PARALLEL WITH RATE LIMITING (%.1f second delay)\n", delay_seconds))
  cat(strrep("=", 80), "\n\n")

  if (!requireNamespace("future", quietly = TRUE)) {
    cat("⚠️  'future' package not installed. Skipping test.\n")
    return(NULL)
  }

  library(future)
  library(future.apply)

  # Setup parallel backend with limited workers
  max_workers <- min(2, length(models))  # Limit concurrent requests
  plan(multisession, workers = max_workers)

  cat(sprintf("Parallel workers: %d (throttled)\n", max_workers))
  cat(sprintf("Delay between batches: %.1f seconds\n\n", delay_seconds))

  start_time <- Sys.time()

  results_list <- future_lapply(seq_along(models), function(i) {
    model <- models[i]

    # Stagger requests
    Sys.sleep((i - 1) * delay_seconds / max_workers)

    model_start <- Sys.time()

    result <- tryCatch({
      annotate_cell_types(
        input = test_clusters,
        tissue_name = tissue_name,
        model = model,
        api_key = api_keys$anthropic,
        top_gene_count = 10
      )
    }, error = function(e) {
      list(error = TRUE, message = e$message, model = model)
    })

    model_duration <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

    return(list(
      model = model,
      result = result,
      duration = model_duration,
      error = !is.null(result$error) && result$error
    ))
  }, future.seed = TRUE)

  total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Reset to sequential
  plan(sequential)

  # Analyze results
  results <- list()
  api_errors <- list()

  for (item in results_list) {
    cat(sprintf("[%s] ", item$model))
    if (item$error) {
      cat(sprintf("❌ ERROR: %s\n", item$result$message))
      api_errors[[item$model]] <- item$result$message
    } else {
      cat(sprintf("✓ Success (%.2f seconds)\n", item$duration))
      results[[item$model]] <- item$result
    }
  }

  cat("\n", strrep("-", 80), "\n")
  cat(sprintf("Total Duration: %.2f seconds\n", total_duration))
  cat(sprintf("Success Rate: %d/%d (%.0f%%)\n",
              length(results), length(models),
              100 * length(results) / length(models)))

  if (length(results) > 0) {
    avg_model_duration <- mean(sapply(results_list[!sapply(results_list, function(x) x$error)],
                                       function(x) x$duration))
    cat(sprintf("Average per model: %.2f seconds\n", avg_model_duration))
  }

  if (length(api_errors) > 0) {
    cat("\n⚠️  ERRORS ENCOUNTERED:\n")
    for (model in names(api_errors)) {
      cat(sprintf("  - %s: %s\n", model, api_errors[[model]]))
    }
  }

  return(list(
    duration = total_duration,
    results = results,
    errors = api_errors,
    success_rate = length(results) / length(models),
    throttle_delay = delay_seconds
  ))
}

# =============================================================================
# Test 4: Adaptive Rate Limiting
# =============================================================================

test_adaptive_rate_limiting <- function() {
  cat("\n", strrep("=", 80), "\n")
  cat("TEST 4: ADAPTIVE RATE LIMITING (WITH BACKOFF)\n")
  cat(strrep("=", 80), "\n\n")

  start_time <- Sys.time()
  results <- list()
  api_errors <- list()

  # Exponential backoff parameters
  base_delay <- 0.5
  max_retries <- 3

  for (i in seq_along(models)) {
    model <- models[i]
    cat(sprintf("[%d/%d] Calling %s...\n", i, length(models), model))

    retry_count <- 0
    success <- FALSE

    while (!success && retry_count < max_retries) {
      if (retry_count > 0) {
        wait_time <- base_delay * (2 ^ (retry_count - 1))
        cat(sprintf("  ⏳ Retry %d after %.1f seconds...\n", retry_count, wait_time))
        Sys.sleep(wait_time)
      }

      model_start <- Sys.time()

      result <- tryCatch({
        annotate_cell_types(
          input = test_clusters,
          tissue_name = tissue_name,
          model = model,
          api_key = api_keys$anthropic,
          top_gene_count = 10
        )
      }, error = function(e) {
        list(error = TRUE, message = e$message)
      })

      model_duration <- as.numeric(difftime(Sys.time(), model_start, units = "secs"))

      if (is.list(result) && !is.null(result$error)) {
        # Check if it's a rate limit error
        if (grepl("rate|limit|429", result$message, ignore.case = TRUE)) {
          cat(sprintf("  ⚠️  Rate limit hit (%.2f seconds)\n", model_duration))
          retry_count <- retry_count + 1
        } else {
          cat(sprintf("  ❌ ERROR: %s\n", result$message))
          api_errors[[model]] <- result$message
          break
        }
      } else {
        cat(sprintf("  ✓ Success (%.2f seconds, %d retries)\n",
                   model_duration, retry_count))
        results[[model]] <- result
        success <- TRUE
      }
    }

    if (!success && retry_count >= max_retries) {
      cat(sprintf("  ❌ Failed after %d retries\n", max_retries))
      api_errors[[model]] <- "Max retries exceeded"
    }

    # Base delay between models
    if (i < length(models)) {
      Sys.sleep(base_delay)
    }
  }

  total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat("\n", strrep("-", 80), "\n")
  cat(sprintf("Total Duration: %.2f seconds\n", total_duration))
  cat(sprintf("Success Rate: %d/%d (%.0f%%)\n",
              length(results), length(models),
              100 * length(results) / length(models)))

  if (length(api_errors) > 0) {
    cat("\nErrors encountered:\n")
    for (model in names(api_errors)) {
      cat(sprintf("  - %s: %s\n", model, api_errors[[model]]))
    }
  }

  return(list(
    duration = total_duration,
    results = results,
    errors = api_errors,
    success_rate = length(results) / length(models)
  ))
}

# =============================================================================
# Run All Tests
# =============================================================================

main <- function() {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                    API RATE LIMIT PERFORMANCE TESTING                     ║\n")
  cat("╚════════════════════════════════════════════════════════════════════════════╝\n")

  all_results <- list()

  # Test 1: Serial (Baseline)
  all_results$serial <- test_serial()

  # Test 2: Parallel (No limits)
  all_results$parallel_no_limit <- test_parallel_no_limit()

  # Test 3: Parallel with throttling (1 second delay)
  all_results$parallel_throttle_1s <- test_parallel_with_throttle(1.0)

  # Test 4: Parallel with throttling (0.5 second delay)
  all_results$parallel_throttle_0.5s <- test_parallel_with_throttle(0.5)

  # Test 5: Adaptive rate limiting
  all_results$adaptive <- test_adaptive_rate_limiting()

  # Summary
  cat("\n", strrep("=", 80), "\n")
  cat("SUMMARY OF ALL TESTS\n")
  cat(strrep("=", 80), "\n\n")

  summary_df <- data.frame(
    Test = character(),
    Duration_sec = numeric(),
    Success_Rate = numeric(),
    Speedup = numeric(),
    Recommendation = character(),
    stringsAsFactors = FALSE
  )

  baseline_duration <- all_results$serial$duration

  for (test_name in names(all_results)) {
    result <- all_results[[test_name]]
    if (!is.null(result)) {
      speedup <- baseline_duration / result$duration

      recommendation <- if (result$success_rate < 1.0) {
        "❌ Not Recommended (API errors)"
      } else if (speedup > 2.0) {
        "✅ Highly Recommended"
      } else if (speedup > 1.3) {
        "✓ Recommended"
      } else {
        "⚠️  Marginal benefit"
      }

      summary_df <- rbind(summary_df, data.frame(
        Test = test_name,
        Duration_sec = round(result$duration, 2),
        Success_Rate = sprintf("%.0f%%", result$success_rate * 100),
        Speedup = sprintf("%.2fx", speedup),
        Recommendation = recommendation,
        stringsAsFactors = FALSE
      ))
    }
  }

  print(summary_df, row.names = FALSE)

  cat("\n", strrep("=", 80), "\n")
  cat("RECOMMENDATIONS FOR IMPLEMENTATION\n")
  cat(strrep("=", 80), "\n\n")

  # Find best strategy
  successful_tests <- summary_df[grepl("100%", summary_df$Success_Rate), ]
  if (nrow(successful_tests) > 0) {
    best_test <- successful_tests[which.min(successful_tests$Duration_sec), ]

    cat("✅ RECOMMENDED STRATEGY:\n")
    cat(sprintf("   Strategy: %s\n", best_test$Test))
    cat(sprintf("   Duration: %.2f seconds\n", best_test$Duration_sec))
    cat(sprintf("   Speedup: %s\n", best_test$Speedup))
    cat(sprintf("   Success: %s\n\n", best_test$Success_Rate))

    if (grepl("throttle", best_test$Test)) {
      throttle_delay <- all_results[[best_test$Test]]$throttle_delay
      cat("   Implementation details:\n")
      cat(sprintf("   - Use parallel processing with max 2 workers\n"))
      cat(sprintf("   - Add %.1f second delay between batches\n", throttle_delay))
      cat(sprintf("   - This provides %.2fx speedup without API overload\n",
                 as.numeric(gsub("x", "", best_test$Speedup))))
    }
  } else {
    cat("⚠️  All parallel strategies encountered API errors.\n")
    cat("   RECOMMENDATION: Stick with serial execution\n")
    cat("   Consider:\n")
    cat("   - Increasing delays between requests\n")
    cat("   - Implementing request queuing\n")
    cat("   - Using different API keys for different models\n")
  }

  cat("\n", strrep("=", 80), "\n\n")

  return(all_results)
}

# Run if sourced directly
if (!interactive()) {
  results <- main()
}
