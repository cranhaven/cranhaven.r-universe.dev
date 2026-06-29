# Real API Integration Tests

test_that("Real API integration with system cache", {
  skip_real_api_test()

  formatted_data <- minimal_pbmc_markers()

  # Test with system cache (NULL)
  result1 <- interactive_consensus_annotation(
    input = formatted_data,
    tissue_name = "human PBMC",
    cache_dir = NULL,
    models = openrouter_test_models,
    api_keys = openrouter_api_keys()
  )

  expect_true(!is.null(result1))
  expect_true("final_annotations" %in% names(result1))

  # Test cache was created in system directory
  cache_dir <- mllmcelltype_cache_dir()
  expect_true(dir.exists(cache_dir))

  # Test local cache in an isolated working directory so the repository is not polluted.
  old_wd <- getwd()
  isolated_wd <- tempfile("mllmcelltype_local_cache_")
  dir.create(isolated_wd)
  on.exit(setwd(old_wd), add = TRUE)
  setwd(isolated_wd)

  result2 <- interactive_consensus_annotation(
    input = formatted_data,
    tissue_name = "human PBMC",
    cache_dir = "local",
    models = openrouter_test_models,
    api_keys = openrouter_api_keys()
  )

  expect_true(!is.null(result2))
  expect_true("final_annotations" %in% names(result2))
  expect_true(dir.exists(file.path(isolated_wd, ".mllmcelltype_cache")))
})

test_that("Cache reuse works with real API", {
  skip_real_api_test()

  test_data <- minimal_pbmc_markers()
  temp_cache <- tempfile("cache_reuse_test_")

  # First call - should hit API
  start_time1 <- Sys.time()
  result1 <- interactive_consensus_annotation(
    input = test_data,
    tissue_name = "human PBMC",
    cache_dir = temp_cache,
    models = openrouter_test_models,
    api_keys = openrouter_api_keys()
  )
  time1 <- as.numeric(Sys.time() - start_time1)

  # Second call - should use cache
  start_time2 <- Sys.time()
  result2 <- interactive_consensus_annotation(
    input = test_data,
    tissue_name = "human PBMC",
    cache_dir = temp_cache,
    models = openrouter_test_models,
    api_keys = openrouter_api_keys()
  )
  time2 <- as.numeric(Sys.time() - start_time2)

  # Cache call should be much faster
  expect_true(time2 < time1 * 0.5)
  expect_equal(result1$final_annotations, result2$final_annotations)
})

test_that("Different cache directories work independently", {
  skip_real_api_test()

  test_data <- data.frame(
    cluster = c(0, 0),
    gene = c("CD3D", "CD8A"),
    avg_log2FC = c(2.5, 2.0)
  )

  cache_dirs <- list(
    temp1 = tempfile("cache_test_1_"),
    temp2 = tempfile("cache_test_2_")
  )

  results <- list()

  for (i in seq_along(cache_dirs)) {
    cache_name <- names(cache_dirs)[i]
    cache_dir <- cache_dirs[[i]]

    result <- interactive_consensus_annotation(
      input = test_data,
      tissue_name = "human PBMC",
      cache_dir = cache_dir,
      models = openrouter_test_models,
      api_keys = openrouter_api_keys()
    )

    results[[cache_name]] <- result

    expect_true(dir.exists(cache_dir))

    cache_files <- list.files(cache_dir, recursive = TRUE)
    expect_true(length(cache_files) > 0)
  }

  expect_equal(results$temp1$final_annotations, results$temp2$final_annotations)
})
