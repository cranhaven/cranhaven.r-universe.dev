# Core Cache Logic Tests

test_that("Cache directory resolution works correctly", {
  # Test NULL (system cache)
  cache_manager <- CacheManager$new(cache_dir = NULL)
  actual_dir <- cache_manager$get_cache_dir()
  expected_pattern <- "mLLMCelltype.*consensus_cache"
  expect_true(grepl(expected_pattern, actual_dir))
  
  # Test "local" option
  cache_manager <- CacheManager$new(cache_dir = "local")
  expect_equal(cache_manager$get_cache_dir(), file.path(".", ".mllmcelltype_cache"))
  
  # Test "temp" option
  cache_manager <- CacheManager$new(cache_dir = "temp")
  expect_true(grepl("mllmcelltype_cache", cache_manager$get_cache_dir()))
  
  # Test custom path
  custom_path <- file.path(tempdir(), "custom_cache_test")
  cache_manager <- CacheManager$new(cache_dir = custom_path)
  expect_equal(cache_manager$get_cache_dir(), custom_path)
})

test_that("Cache directory creation follows CRAN policies", {
  # Save current working directory
  original_wd <- getwd()
  
  # Create a temporary directory to test in
  test_dir <- tempfile()
  dir.create(test_dir)
  setwd(test_dir)
  
  # Verify no directory created in current working directory by default
  initial_files <- list.files(".", all.files = TRUE)
  cache_manager <- CacheManager$new()
  after_files <- list.files(".", all.files = TRUE)
  
  expect_false("consensus_cache" %in% after_files)
  expect_equal(length(initial_files), length(after_files))
  
  # Restore working directory
  setwd(original_wd)
  unlink(test_dir, recursive = TRUE)
})

test_that("Utility functions work correctly", {
  # Test mllmcelltype_cache_dir()
  system_cache <- mllmcelltype_cache_dir()
  expect_true(dir.exists(system_cache))
  
  local_cache <- mllmcelltype_cache_dir("local")
  expect_equal(local_cache, file.path(".", ".mllmcelltype_cache"))
  
  # Test with temp option
  temp_cache <- mllmcelltype_cache_dir("temp")
  expect_true(grepl("mllmcelltype_cache", temp_cache))
})

test_that("Cache key generation works", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())
  
  # Test with simple data
  test_data <- data.frame(
    CD3D = c(2.1, 0.1, 3.2),
    CD8A = c(1.5, 0.0, 2.8),
    IL7R = c(0.8, 2.3, 1.1)
  )
  
  key1 <- cache_manager$generate_key(test_data, "gpt-5.5", "cluster_0")
  key2 <- cache_manager$generate_key(test_data, "gpt-5.5", "cluster_1")
  key3 <- cache_manager$generate_key(test_data, "claude-sonnet-4-6", "cluster_0")

  # Keys should be different for different inputs
  expect_true(nchar(key1) > 0)
  expect_false(key1 == key2)  # Different cluster
  expect_false(key1 == key3)  # Different model
})

test_that("Cache key varies with tissue_name and top_gene_count", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())

  test_data <- data.frame(
    cluster = c(0, 0, 1, 1),
    gene = c("CD3D", "CD8A", "CD14", "LYZ"),
    avg_log2FC = c(2.5, 2.1, 1.8, 1.6)
  )

  base_key <- cache_manager$generate_key(test_data, "gpt-5.5", "0",
                                          tissue_name = "human PBMC", top_gene_count = 10)
  diff_tissue <- cache_manager$generate_key(test_data, "gpt-5.5", "0",
                                             tissue_name = "mouse brain", top_gene_count = 10)
  diff_topn <- cache_manager$generate_key(test_data, "gpt-5.5", "0",
                                           tissue_name = "human PBMC", top_gene_count = 5)
  same_key <- cache_manager$generate_key(test_data, "gpt-5.5", "0",
                                          tissue_name = "human PBMC", top_gene_count = 10)

  expect_false(base_key == diff_tissue)  # Different tissue
  expect_false(base_key == diff_topn)    # Different top_gene_count
  expect_equal(base_key, same_key)       # Same parameters = same key
})