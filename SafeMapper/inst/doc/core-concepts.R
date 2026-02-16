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
# These two calls will generate the same fingerprint
data <- 1:100

result1 <- s_map(data, ~ .x^2)
# If re-run immediately, same fingerprint will be detected

# This call generates a different fingerprint (different data)
data2 <- 1:200
result2 <- s_map(data2, ~ .x^2)

## -----------------------------------------------------------------------------
# Use custom session ID
result <- s_map(1:20, ~ .x^2, .session_id = "my_custom_session")

# This ensures that even tasks with similar data features won't conflict

## -----------------------------------------------------------------------------
# Default batch size is 100
# For fast operations, increase batch size to reduce I/O
s_configure(batch_size = 200)

# For slow operations (like API calls), decrease batch size for more frequent saves
s_configure(batch_size = 10)

# Reset to defaults
s_configure(batch_size = 100)

## -----------------------------------------------------------------------------
# Simulate a task that might be interrupted
simulate_task <- function(x) {
  Sys.sleep(0.01)
  x^2
}

# First run
result <- s_map(1:30, simulate_task, .session_id = "recovery_demo")

# If task is interrupted, simply re-run the same code:
# result <- s_map(1:30, simulate_task, .session_id = "recovery_demo")
# Output: "Resuming from item XX/30"

## -----------------------------------------------------------------------------
# For unstable network environments, increase retry attempts
s_configure(retry_attempts = 5)

# For stable local computations, reduce retry attempts
s_configure(retry_attempts = 1)

# Reset to default
s_configure(retry_attempts = 3)

## -----------------------------------------------------------------------------
# Checkpoint storage location (varies by system)
# Linux:   ~/.cache/R/SafeMapper/checkpoints/
# macOS:   ~/Library/Caches/org.R-project.R/R/SafeMapper/checkpoints/
# Windows: %LOCALAPPDATA%/R/cache/R/SafeMapper/checkpoints/

