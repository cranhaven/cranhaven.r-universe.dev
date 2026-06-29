# Cache Performance Tests

test_that("Cache key generation performance", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Test with moderately large dataset
  large_data <- data.frame(
    cluster = sample(0:9, 1000, replace = TRUE),  # 10 clusters
    gene = paste0("Gene_", sample(1:500, 1000, replace = TRUE)),  # 500 unique genes
    avg_log2FC = rnorm(1000, mean = 1.5, sd = 0.8)
  )
  
  # Key generation should be fast even for large data
  start_time <- Sys.time()
  key <- cache_manager$generate_key(large_data, "gpt-5.5", "0")
  key_time <- as.numeric(Sys.time() - start_time)
  
  expect_true(key_time < 2.0)  # Should take less than 2 seconds
  expect_true(nchar(key) > 0)
  expect_true(grepl("^v_", key))  # Should start with version prefix
})

test_that("Cache I/O performance", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Create moderately large result object
  large_result <- list(
    annotation = "T cell",
    discussion_log = list(
      rounds = replicate(3, list(
        model_responses = replicate(5, list(
          response = paste(sample(letters, 200, replace = TRUE), collapse = ""),
          reasoning = paste(sample(LETTERS, 300, replace = TRUE), collapse = "")
        ), simplify = FALSE),
        consensus_result = list(majority_prediction = "T cell")
      ), simplify = FALSE)
    ),
    confidence_score = 0.95,
    metadata = list(
      processing_time = 45.2,
      models_used = c("gpt-5.5", "claude-sonnet-4-6", "gemini-3.1-pro-preview"),
      gene_count = 50
    )
  )
  
  key <- "performance_test_key"
  
  # Save performance
  start_time <- Sys.time()
  cache_manager$save_to_cache(key, large_result)
  save_time <- as.numeric(Sys.time() - start_time)
  
  # Load performance  
  start_time <- Sys.time()
  loaded <- cache_manager$load_from_cache(key)
  load_time <- as.numeric(Sys.time() - start_time)
  
  expect_true(save_time < 1.0)  # Save should be fast
  expect_true(load_time < 0.2)  # Load should be very fast
  expect_equal(loaded$annotation, large_result$annotation)
  expect_equal(length(loaded$discussion_log$rounds), 3)
})

test_that("Cache statistics and management performance", {
  # Use a unique cache directory to avoid interference
  unique_cache_dir <- file.path(tempdir(), paste0("perf_test_", Sys.getpid(), "_", as.numeric(Sys.time())))
  cache_manager <- CacheManager$new(cache_dir = unique_cache_dir)
  
  # Create multiple cache entries
  for (i in 1:20) {
    test_result <- list(
      annotation = paste("Cell_type", i),
      discussion_log = list(rounds = list(list(response = paste("Response", i)))),
      confidence = runif(1)
    )
    key <- paste0("perf_test_", i)
    cache_manager$save_to_cache(key, test_result)
  }
  
  # Test cache statistics performance
  start_time <- Sys.time()
  stats <- cache_manager$get_cache_stats()
  stats_time <- as.numeric(Sys.time() - start_time)
  
  expect_true(stats_time < 0.5)  # Should be fast
  expect_equal(stats$cache_count, 20)
  expect_true(stats$cache_size_mb > 0)
  expect_true(stats$cache_exists)
  
  # Test cache clearing performance
  start_time <- Sys.time()
  cache_manager$clear_cache(confirm = TRUE)
  clear_time <- as.numeric(Sys.time() - start_time)
  
  expect_true(clear_time < 1.0)  # Should be fast
  
  # Verify cache is cleared
  final_stats <- cache_manager$get_cache_stats()
  expect_equal(final_stats$cache_count, 0)
})

test_that("Different cache directory modes performance", {
  # Test all cache directory modes
  cache_modes <- list(
    system = NULL,
    local = "local",
    temp = "temp",
    custom = file.path(tempdir(), "custom_perf_test")
  )
  
  results <- list()
  
  for (mode_name in names(cache_modes)) {
    cache_dir <- cache_modes[[mode_name]]
    
    # Test cache manager creation time
    start_time <- Sys.time()
    cache_manager <- CacheManager$new(cache_dir = cache_dir)
    creation_time <- as.numeric(Sys.time() - start_time)
    
    # Cache manager creation should be fast
    expect_true(creation_time < 0.5)
    
    # Test directory exists
    actual_dir <- cache_manager$get_cache_dir()
    expect_true(dir.exists(actual_dir))
    
    # Test basic operation
    test_data <- list(annotation = paste("Test", mode_name))
    key <- paste0("mode_test_", mode_name)
    
    cache_manager$save_to_cache(key, test_data)
    expect_true(cache_manager$has_cache(key))
    
    loaded <- cache_manager$load_from_cache(key)
    expect_equal(loaded$annotation, test_data$annotation)
    
    results[[mode_name]] <- list(
      creation_time = creation_time,
      cache_dir = actual_dir
    )
  }
  
  # All modes should have reasonable performance
  creation_times <- sapply(results, function(x) x$creation_time)
  expect_true(all(creation_times < 0.5))
  
  # All cache directories should exist and be different
  cache_dirs <- sapply(results, function(x) x$cache_dir)
  expect_equal(length(unique(cache_dirs)), length(cache_dirs))
})

test_that("Cache validation performance with various data sizes", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Test validation with different sized objects
  sizes <- c(
    small = 10,    # Small result
    medium = 100,  # Medium result  
    large = 500    # Large result
  )
  
  validation_times <- list()
  
  for (size_name in names(sizes)) {
    size_val <- sizes[[size_name]]
    
    # Create test data of specified size
    test_result <- list(
      annotation = paste("Test annotation", size_name),
      discussion_log = list(
        rounds = replicate(3, list(
          model_responses = replicate(size_val, list(
            response = paste(sample(letters, 20, replace = TRUE), collapse = "")
          ), simplify = FALSE),
          consensus_result = list(majority_prediction = "Test cell")
        ), simplify = FALSE)
      )
    )
    
    key <- paste0("validation_test_", size_name)
    cache_manager$save_to_cache(key, test_result)
    
    # Test validation performance
    start_time <- Sys.time()
    is_valid <- cache_manager$validate_cache(key)
    validation_time <- as.numeric(Sys.time() - start_time)
    
    expect_true(is_valid)
    expect_true(validation_time < 0.1)  # Should be very fast
    
    validation_times[[size_name]] <- validation_time
  }
  
  # Validation should be consistently fast regardless of data size
  expect_true(all(unlist(validation_times) < 0.1))
})