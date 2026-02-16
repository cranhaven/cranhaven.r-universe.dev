## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # From r-universe (recommended)
# install.packages("SafeMapper", repos = "https://zaoqu-liu.r-universe.dev")
# 
# # Or from GitHub
# devtools::install_github("Zaoqu-Liu/SafeMapper")

## -----------------------------------------------------------------------------
library(SafeMapper)

## ----eval=FALSE---------------------------------------------------------------
# # Traditional: if interrupted, all progress is lost
# library(purrr)
# result <- map(1:100, function(x) {
#   Sys.sleep(0.1)  # Simulate slow operation
#   x^2
# })

## -----------------------------------------------------------------------------
# SafeMapper: automatically saves progress
result <- s_map(1:20, function(x) {
  Sys.sleep(0.01)  # Simulate slow operation
  x^2
})

# View results
head(unlist(result), 10)

## -----------------------------------------------------------------------------
# First run
result1 <- s_map(1:50, ~ .x * 2, .session_id = "demo_recovery")

# If interrupted, just run the same code again to resume
# result2 <- s_map(1:50, ~ .x * 2, .session_id = "demo_recovery")
# Output: "Resuming from item XX/50"

## -----------------------------------------------------------------------------
# Return character vector
char_result <- s_map_chr(c("a", "b", "c"), toupper)
print(char_result)

# Return numeric vector
num_result <- s_map_dbl(1:5, ~ .x^2)
print(num_result)

# Return logical vector
lgl_result <- s_map_lgl(1:5, ~ .x > 3)
print(lgl_result)

## -----------------------------------------------------------------------------
# s_map2: process two vectors simultaneously
x <- 1:5
y <- 6:10
sums <- s_map2_dbl(x, y, ~ .x + .y)
print(sums)

## -----------------------------------------------------------------------------
# s_pmap: process multiple inputs
params <- list(
  a = 1:3,
  b = 4:6,
  c = 7:9
)
products <- s_pmap(params, ~ ..1 * ..2 * ..3)
print(unlist(products))

## -----------------------------------------------------------------------------
# Adjust batch size and retry attempts
s_configure(
  batch_size = 50,      # Save checkpoint every 50 items
  retry_attempts = 5    # Retry 5 times on failure
)

## ----eval=FALSE---------------------------------------------------------------
# # Clean checkpoint files older than 7 days
# s_clean_sessions(older_than_days = 7)

