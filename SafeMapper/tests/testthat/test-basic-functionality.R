# =============================================================================
# SafeMapper Unit Tests
# =============================================================================

# --- Basic s_map Functions ---

test_that("s_map produces correct results", {
  result <- s_map(1:5, function(x) x^2)
  expect_equal(result, list(1, 4, 9, 16, 25))
})

test_that("s_map works with formula syntax", {
  result <- s_map(1:5, ~ .x * 2)
  expect_equal(result, list(2, 4, 6, 8, 10))
})

test_that("s_map handles single element", {
  result <- s_map(5, ~ .x^2)
  expect_equal(result, list(25))
})

test_that("s_map handles additional arguments", {
  add_n <- function(x, n) x + n
  result <- s_map(1:3, add_n, n = 10)
  expect_equal(result, list(11, 12, 13))
})

# --- Type-specific map variants ---

test_that("s_map_chr returns character vector", {
  result <- s_map_chr(1:3, ~ paste("item", .x))
  expect_type(result, "character")
  expect_equal(result, c("item 1", "item 2", "item 3"))
})

test_that("s_map_dbl returns double vector", {
  result <- s_map_dbl(1:5, ~ .x * 1.5)
  expect_type(result, "double")
  expect_equal(result, c(1.5, 3.0, 4.5, 6.0, 7.5))
})

test_that("s_map_int returns integer vector", {
  result <- s_map_int(1:5, ~ as.integer(.x * 2))
  expect_type(result, "integer")
  expect_equal(result, c(2L, 4L, 6L, 8L, 10L))
})

test_that("s_map_lgl returns logical vector", {
  result <- s_map_lgl(1:5, ~ .x > 3)
  expect_type(result, "logical")
  expect_equal(result, c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("s_map_dfr returns row-bound data frame", {
  result <- s_map_dfr(1:3, ~ data.frame(id = .x, value = .x^2))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$value, c(1, 4, 9))
})

test_that("s_map_dfc returns column-bound data frame", {
  result <- s_map_dfc(1:3, ~ setNames(data.frame(.x^2), paste0("col", .x)))
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
})

# --- s_map2 Functions ---

test_that("s_map2 works with two inputs", {
  result <- s_map2(1:3, 4:6, `+`)
  expect_equal(result, list(5L, 7L, 9L))
})

test_that("s_map2_dbl returns double vector", {
  result <- s_map2_dbl(1:3, 1:3, `*`)
  expect_type(result, "double")
  expect_equal(result, c(1, 4, 9))
})

test_that("s_map2 handles formula syntax", {
  result <- s_map2(1:3, 10:12, ~ .x + .y)
  expect_equal(result, list(11, 13, 15))
})

# --- s_pmap Functions ---

test_that("s_pmap works with multiple inputs", {
  data <- list(a = 1:3, b = 4:6, c = 7:9)
  result <- s_pmap(data, function(a, b, c) a + b + c)
  expect_equal(result, list(12, 15, 18))
})

test_that("s_pmap validates input", {
  expect_error(s_pmap(list(), identity), ".l must be a non-empty list")
})

# --- s_walk Functions ---

test_that("s_walk executes side effects", {
  accumulator <- c()
  s_walk(1:3, function(x) accumulator <<- c(accumulator, x))
  expect_equal(accumulator, 1:3)
})

test_that("s_walk returns input invisibly", {
  result <- s_walk(1:3, identity)
  expect_equal(result, 1:3)
})

test_that("s_walk2 executes with two inputs", {
  accumulator <- c()
  s_walk2(1:3, 4:6, function(x, y) accumulator <<- c(accumulator, x + y))
  expect_equal(accumulator, c(5, 7, 9))
})

# --- s_imap Functions ---

test_that("s_imap provides indices", {
  result <- s_imap(c("a", "b", "c"), ~ paste(.y, .x, sep = ":"))
  expect_equal(result, list("1:a", "2:b", "3:c"))
})

test_that("s_imap_chr returns character vector", {
  result <- s_imap_chr(c("x", "y"), ~ paste0("[", .y, "]", .x))
  expect_type(result, "character")
  expect_equal(result, c("[1]x", "[2]y"))
})

# --- Error Handling Functions ---

test_that("s_safely captures errors", {
  safe_log <- s_safely(log)
  
  r1 <- safe_log(10)
  expect_false(is.null(r1$result))
  expect_null(r1$error)
  
  r2 <- safe_log("invalid")
  expect_null(r2$result)
  expect_false(is.null(r2$error))
})

test_that("s_possibly returns default on error", {
  possible_log <- s_possibly(log, otherwise = NA)
  
  expect_equal(possible_log(exp(1)), 1)
  expect_true(is.na(possible_log("invalid")))
})

test_that("s_quietly captures output", {
  quiet_fn <- s_quietly(function(x) {
    message("msg")
    x * 2
  })
  
  result <- quiet_fn(5)
  expect_equal(result$result, 10)
  expect_length(result$messages, 1)
})

# --- Configuration ---

test_that("s_configure sets options", {
  s_configure(batch_size = 25, retry_attempts = 5)
  
  config <- .get_config()
  expect_equal(config$batch_size, 25L)
  expect_equal(config$retry_attempts, 5L)
  
  # Reset

  s_configure(batch_size = 100L, retry_attempts = 3L)
})

# --- Session Management ---
  
test_that("s_list_sessions returns data frame", {
  sessions <- s_list_sessions()
  expect_s3_class(sessions, "data.frame")
  expect_true(all(c("session_id", "created", "items_completed", 
                    "total_items", "completion_rate", "status") %in% names(sessions)))
})

test_that("s_session_stats runs without error", {
  expect_no_error(s_session_stats())
})

test_that("s_clean_sessions handles empty case", {
  expect_no_error(s_clean_sessions(older_than_days = 365))
})

# --- Fingerprint Mechanism ---

test_that("fingerprint is stable for identical data", {
  fp1 <- .make_fingerprint(list(1:100), "map")
  fp2 <- .make_fingerprint(list(1:100), "map")
  expect_equal(fp1, fp2)
})

test_that("fingerprint differs for different data", {
  fp1 <- .make_fingerprint(list(1:100), "map")
  fp2 <- .make_fingerprint(list(1:101), "map")
  expect_false(fp1 == fp2)
})

test_that("fingerprint differs for different modes", {
  fp1 <- .make_fingerprint(list(1:10), "map")
  fp2 <- .make_fingerprint(list(1:10), "map2")
  expect_false(fp1 == fp2)
})

# --- Recovery Mechanism ---

test_that("checkpoint saves and restores correctly", {
  session_id <- paste0("test_checkpoint_", format(Sys.time(), "%Y%m%d%H%M%S"))
  s_configure(batch_size = 5, retry_attempts = 1)
  
  # First run - fail at item 8
  counter <- 0
  fail_fn <- function(x) {
    counter <<- counter + 1
    if (counter == 8) stop("Planned failure")
    x^2
  }
  
  expect_error(s_map(1:15, fail_fn, .session_id = session_id))
  
  # Verify checkpoint exists
  sessions <- s_list_sessions()
  expect_true(session_id %in% sessions$session_id)
  
  # Second run - should resume
  success_fn <- function(x) x^2
  result <- s_map(1:15, success_fn, .session_id = session_id)
  
  expect_equal(result, as.list((1:15)^2))
  
  # Cleanup
  s_clean_sessions(session_ids = session_id)
  s_configure(batch_size = 100L, retry_attempts = 3L)
})

test_that("auto recovery works without session_id", {
  s_configure(batch_size = 5)
  
  # Use unique data that won't collide with other tests
  test_data <- 1001:1010
  
  result <- s_map(test_data, ~ .x^2)
  expect_equal(result, as.list(test_data^2))
  
  # Checkpoint should be cleaned after success
  fingerprint <- .make_fingerprint(list(test_data), "map")
  sessions <- s_list_sessions()
  expect_false(fingerprint %in% sessions$session_id)
  
  s_configure(batch_size = 100L)
})

# --- Edge Cases ---

test_that("empty input returns empty list", {
  result <- s_map(list(), identity)
  expect_equal(result, list())
})

test_that("NULL values are preserved", {
  result <- s_map(1:3, function(x) if (x == 2) NULL else x)
  expect_null(result[[2]])
  expect_equal(result[[1]], 1)
  expect_equal(result[[3]], 3)
})

test_that("NA values are handled", {
  result <- s_map(c(1, NA, 3), function(x) if (is.na(x)) 0 else x)
  expect_equal(result, list(1, 0, 3))
})

test_that("vectors of different lengths cause error in map2", {
  expect_error(s_map2(1:3, 1:4, `+`))
})

# --- Parallel Functions (sequential plan) ---

test_that("s_future_map works with sequential plan", {
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  
  library(future)
  plan(sequential)
  
  result <- s_future_map(1:10, ~ .x^2)
  expect_equal(result, as.list((1:10)^2))
})

test_that("s_future_map2 works with sequential plan", {
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  
  library(future)
  plan(sequential)
  
  result <- s_future_map2(1:5, 6:10, `+`)
  expect_equal(result, as.list((1:5) + (6:10)))
})

test_that("s_future_pmap works with sequential plan", {
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  
  library(future)
  plan(sequential)
  
  result <- s_future_pmap(list(a = 1:3, b = 4:6), function(a, b) a + b)
  expect_equal(result, list(5, 7, 9))
})
