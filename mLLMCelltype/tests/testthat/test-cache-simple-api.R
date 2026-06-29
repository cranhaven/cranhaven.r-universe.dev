# Simplified Cache Test

test_that("Cache directory creation works with simple test", {
  skip_if_not_cran_env()

  # Test system cache directory creation
  system_cache <- mllmcelltype_cache_dir()
  expect_true(dir.exists(system_cache))
  expect_true(grepl("mLLMCelltype", system_cache))
  expect_true(grepl("consensus_cache", system_cache))
  
  # Test local cache directory
  local_cache <- mllmcelltype_cache_dir("local") 
  expect_equal(local_cache, file.path(".", ".mllmcelltype_cache"))
  
  # Test temp cache directory
  temp_cache <- mllmcelltype_cache_dir("temp")
  expect_true(grepl("mllmcelltype_cache", temp_cache))
  
  # Test cache manager with different modes
  cm_system <- CacheManager$new(cache_dir = NULL)
  expect_true(dir.exists(cm_system$get_cache_dir()))
  
  cm_local <- CacheManager$new(cache_dir = "local")
  expect_equal(cm_local$get_cache_dir(), file.path(".", ".mllmcelltype_cache"))
  
  cm_temp <- CacheManager$new(cache_dir = "temp") 
  expect_true(dir.exists(cm_temp$get_cache_dir()))
})

test_that("Cache key generation works correctly", {
  skip_if_not_cran_env()
  
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Test data in expected format
  test_data <- data.frame(
    cluster = c(0, 0, 1, 1),
    gene = c("CD3D", "CD8A", "CD14", "LYZ"),
    avg_log2FC = c(2.5, 2.1, 1.8, 1.6)
  )
  
  # Generate keys for different scenarios
  key1 <- cache_manager$generate_key(test_data, "openai/gpt-4o-mini", "0")
  key2 <- cache_manager$generate_key(test_data, "openai/gpt-4o-mini", "1") 
  key3 <- cache_manager$generate_key(test_data, "claude-haiku-4-5-20251001", "0")
  
  # Keys should be different
  expect_true(nchar(key1) > 0)
  expect_true(nchar(key2) > 0)
  expect_true(nchar(key3) > 0)
  expect_false(key1 == key2)  # Different cluster
  expect_false(key1 == key3)  # Different model
  
  # Same parameters should generate same key
  key1_repeat <- cache_manager$generate_key(test_data, "openai/gpt-4o-mini", "0")
  expect_equal(key1, key1_repeat)
})

test_that("Cache save and load operations work", {
  skip_if_not_cran_env()
  
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Mock result data
  test_result <- list(
    annotation = "T cell",
    discussion_log = list(
      rounds = list(
        list(model = "gpt-5.5", response = "T cell")
      )
    ),
    confidence = 0.95
  )
  
  key <- "test_save_load"
  
  # Test save
  cache_manager$save_to_cache(key, test_result)
  expect_true(cache_manager$has_cache(key))
  
  # Test load
  loaded_result <- cache_manager$load_from_cache(key)
  expect_equal(loaded_result$annotation, "T cell")
  expect_equal(loaded_result$confidence, 0.95)
  
  # Test validation
  expect_true(cache_manager$validate_cache(key))
})