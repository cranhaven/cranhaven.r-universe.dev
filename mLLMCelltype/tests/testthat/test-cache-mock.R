# Mock API Response Tests

test_that("Cache saves and retrieves mock consensus results", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Mock input data
  mock_input <- data.frame(
    gene = c("CD3D", "CD8A", "IL7R"),
    cluster = c("T_cells", "CD8_T", "Memory_T")
  )
  
  mock_consensus <- list(
    consensus_annotation = "T cell",
    confidence_score = 0.95,
    models_used = c("gpt-5.5", "claude-sonnet-4-6"),
    timestamp = Sys.time()
  )
  
  # Generate cache key
  key <- cache_manager$generate_key(mock_input, c("gpt-5.5", "claude-sonnet-4-6"), "cluster_1")
  
  # Test saving
  cache_manager$save_to_cache(key, mock_consensus)
  expect_true(cache_manager$has_cache(key))
  
  # Test loading
  loaded_data <- cache_manager$load_from_cache(key)
  expect_equal(loaded_data$consensus_annotation, "T cell")
  expect_equal(loaded_data$confidence_score, 0.95)
})

test_that("Cache handles different input formats", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Test with data.frame format that matches expected structure
  df_input <- data.frame(
    cluster = c(0, 0, 1, 1),
    gene = c("CD3D", "CD8A", "CD14", "LYZ"),
    avg_log2FC = c(2.1, 1.5, 0.8, 1.2)
  )
  
  # Test with simplified data.frame
  simple_input <- data.frame(
    CD3D = c(2.1, 0.1),
    CD8A = c(1.5, 0.0),
    row.names = c("Cell1", "Cell2")
  )
  
  # Both should generate valid cache keys
  key1 <- cache_manager$generate_key(df_input, "gpt-5.5", "0")
  key2 <- cache_manager$generate_key(simple_input, "gpt-5.5", "0")
  
  expect_true(nchar(key1) > 0)
  expect_true(nchar(key2) > 0)
  # Keys should be different for different input structures
  expect_false(key1 == key2)
})

test_that("Cache validation works", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Test with valid data that matches expected structure
  valid_result <- list(
    annotation = "T cell",
    discussion_log = list(
      rounds = list(
        list(model = "gpt-5.5", response = "T cell")
      )
    ),
    confidence_score = 0.95
  )
  
  key <- "test_validation_key"
  cache_manager$save_to_cache(key, valid_result)
  
  # Validation should pass
  expect_true(cache_manager$validate_cache(key))
  
  # Test with invalid data structure
  invalid_result <- list(
    consensus_annotation = "T cell",  # Wrong field name
    confidence_score = 0.95
  )
  
  invalid_key <- "test_invalid_key"
  cache_manager$save_to_cache(invalid_key, invalid_result)
  
  # Validation should fail
  expect_false(cache_manager$validate_cache(invalid_key))
  
  # Test with non-existent key
  expect_false(cache_manager$validate_cache("non_existent_key"))
})