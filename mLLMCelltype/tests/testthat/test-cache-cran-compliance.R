# CRAN Compliance and Cross-Platform Tests

test_that("No directory pollution in current working directory", {
  # Save current working directory
  original_wd <- getwd()
  
  # Create a clean temporary directory to test in
  test_dir <- tempfile()
  dir.create(test_dir)
  setwd(test_dir)
  
  # Record initial files
  initial_files <- list.files(".", all.files = TRUE)
  initial_files <- initial_files[!initial_files %in% c(".", "..")]
  
  # Load the package and create cache manager with default settings
  # This should NOT create any files in current directory
  cache_manager <- CacheManager$new()  # Default: NULL cache_dir
  
  # Check current directory is clean
  after_files <- list.files(".", all.files = TRUE) 
  after_files <- after_files[!after_files %in% c(".", "..")]
  
  # Should be no new files in current directory
  expect_equal(length(after_files), length(initial_files))
  expect_false("consensus_cache" %in% after_files)
  expect_false(".mllmcelltype_cache" %in% after_files)
  
  # Verify cache was created in system location instead
  system_cache <- cache_manager$get_cache_dir()
  expect_true(dir.exists(system_cache))
  expect_false(startsWith(system_cache, test_dir))  # Not in test directory
  
  # Test with utility functions too
  system_cache2 <- mllmcelltype_cache_dir()
  expect_true(dir.exists(system_cache2))
  expect_false(startsWith(system_cache2, test_dir))  # Not in test directory
  
  # Cleanup and restore working directory
  setwd(original_wd)
  unlink(test_dir, recursive = TRUE)
})

test_that("System cache directories follow OS conventions", {
  system_cache <- mllmcelltype_cache_dir()
  
  # Check that system cache follows OS conventions
  os_type <- Sys.info()["sysname"]
  
  if (os_type == "Darwin") {  # macOS
    expect_true(grepl("Library/Caches", system_cache) || grepl("\\.cache", system_cache))
  } else if (os_type == "Linux") {
    expect_true(grepl("\\.cache", system_cache) || grepl("/cache/", system_cache))
  } else if (os_type == "Windows") {
    expect_true(grepl("AppData.*Local", system_cache) || grepl("cache", system_cache, ignore.case = TRUE))
  }
  
  # Should always contain package identifier
  expect_true(grepl("mLLMCelltype", system_cache, ignore.case = TRUE))
  expect_true(grepl("consensus_cache", system_cache))
})

test_that("Package startup message handles old cache detection", {
  # Save original working directory
  original_wd <- getwd()

  # Create temporary test directory
  test_dir <- tempfile()
  dir.create(test_dir)
  setwd(test_dir)

  # Create old cache directory
  old_cache_dir <- file.path(".", "consensus_cache")
  dir.create(old_cache_dir)
  writeLines("old cache content", file.path(old_cache_dir, "test.txt"))

  # Capture packageStartupMessage conditions directly
  # (capture.output doesn't reliably capture packageStartupMessage
  # because testthat's condition handler intercepts them first)
  captured_messages <- character(0)
  withCallingHandlers(
    .onAttach("mLLMCelltype", "mLLMCelltype"),
    packageStartupMessage = function(m) {
      captured_messages <<- c(captured_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Should contain old cache detection message
  all_messages <- paste(captured_messages, collapse = " ")
  expect_true(grepl("Found old cache directory", all_messages))
  expect_true(grepl("mllmcelltype_cache_dir", all_messages))

  # Cleanup
  setwd(original_wd)
  unlink(test_dir, recursive = TRUE)
})

test_that("Build ignore patterns are correct", {
  rbuildignore_path <- file.path("R", ".Rbuildignore")
  
  if (file.exists(rbuildignore_path)) {
    rbuildignore <- readLines(rbuildignore_path)
    
    # Should ignore various cache directories
    cache_patterns <- c(
      "mllmcelltype_cache",
      "consensus_cache"
    )
    
    for (pattern in cache_patterns) {
      pattern_found <- any(grepl(pattern, rbuildignore))
      expect_true(pattern_found, info = paste("Pattern", pattern, "not found in .Rbuildignore"))
    }
  }
})

test_that("Cache directories are properly isolated", {
  # Test that different cache modes create separate directories
  cache_modes <- list(
    system = NULL,
    local = "local", 
    temp = "temp",
    custom = file.path(tempdir(), "custom_isolation_test")
  )
  
  cache_dirs <- list()
  cache_managers <- list()
  
  for (mode_name in names(cache_modes)) {
    cache_dir <- cache_modes[[mode_name]]
    cache_manager <- CacheManager$new(cache_dir = cache_dir)
    
    cache_managers[[mode_name]] <- cache_manager
    cache_dirs[[mode_name]] <- cache_manager$get_cache_dir()
    
    # Each should create a separate directory
    expect_true(dir.exists(cache_dirs[[mode_name]]))
  }
  
  # All cache directories should be different
  unique_dirs <- unique(unlist(cache_dirs))
  expect_equal(length(unique_dirs), length(cache_dirs))
  
  # Test that cache operations are isolated
  for (mode_name in names(cache_managers)) {
    manager <- cache_managers[[mode_name]]
    
    test_data <- list(
      annotation = paste("Test", mode_name),
      mode = mode_name
    )
    key <- paste0("isolation_test_", mode_name)
    
    manager$save_to_cache(key, test_data)
    expect_true(manager$has_cache(key))
    
    # Other managers should not see this cache
    for (other_mode in names(cache_managers)) {
      if (other_mode != mode_name) {
        other_manager <- cache_managers[[other_mode]]
        expect_false(other_manager$has_cache(key), 
                    info = paste("Cache leaked from", mode_name, "to", other_mode))
      }
    }
  }
})

test_that("Cache operations work across different R sessions", {
  # Simulate cross-session cache usage
  temp_cache_dir <- file.path(tempdir(), "cross_session_test")

  # Session 1: Create cache with data matching validate_cache() schema
  # (validate_cache requires: annotation + discussion_log with rounds)
  cache_manager1 <- CacheManager$new(cache_dir = temp_cache_dir)

  test_data <- list(
    annotation = "Cross-session test",
    discussion_log = list(
      cluster_id = "0",
      rounds = list(
        list(round_number = 1, responses = list(model1 = "T cells"))
      )
    )
  )

  key <- "cross_session_key"
  cache_manager1$save_to_cache(key, test_data)
  expect_true(cache_manager1$has_cache(key))

  # Session 2: Create new manager instance (simulates new R session)
  cache_manager2 <- CacheManager$new(cache_dir = temp_cache_dir)

  # Should be able to access cache from previous "session"
  expect_true(cache_manager2$has_cache(key))

  loaded_data <- cache_manager2$load_from_cache(key)
  expect_equal(loaded_data$annotation, test_data$annotation)
  expect_equal(loaded_data$discussion_log$cluster_id, "0")

  # Should be able to validate cache
  expect_true(cache_manager2$validate_cache(key))
})

test_that("Memory usage is reasonable for cache operations", {
  cache_manager <- CacheManager$new(cache_dir = tempdir())

  # Create test data of various sizes
  small_data <- list(annotation = "Small test")
  medium_data <- list(
    annotation = "Medium test",
    details = rep("test data", 100)
  )
  large_data <- list(
    annotation = "Large test",
    details = rep("test data with more content", 1000)
  )

  test_cases <- list(
    small = small_data,
    medium = medium_data,
    large = large_data
  )

  for (size_name in names(test_cases)) {
    data <- test_cases[[size_name]]
    key <- paste0("memory_test_", size_name)

    # Cross-platform memory measurement using gc()
    # gc()[,2] returns "used (Mb)" for Ncells and Vcells
    gc(verbose = FALSE, reset = TRUE)
    mem_before <- sum(gc(verbose = FALSE)[, 2])

    cache_manager$save_to_cache(key, data)
    loaded_data <- cache_manager$load_from_cache(key)

    gc(verbose = FALSE)
    mem_after <- sum(gc(verbose = FALSE)[, 2])

    # Memory increase should be reasonable (less than 10MB for our test data)
    mem_increase <- mem_after - mem_before
    expect_true(mem_increase < 10,
                info = sprintf("Memory increased by %.1f MB for %s data", mem_increase, size_name))

    # Loaded data should match original
    expect_equal(loaded_data$annotation, data$annotation)
  }
})