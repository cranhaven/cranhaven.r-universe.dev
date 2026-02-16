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
# View current defaults by calling with no arguments behavior
# Default configuration:
# - batch_size = 100
# - retry_attempts = 3
# - auto_recover = TRUE

# Customize settings
s_configure(
  batch_size = 50,       # Items per checkpoint
  retry_attempts = 5,    # Retries for failed batches
  auto_recover = TRUE    # Enable automatic recovery
)

## -----------------------------------------------------------------------------
# For API calls (slow, potentially unstable)
s_configure(
  batch_size = 20,      # Save frequently
  retry_attempts = 5    # Handle transient errors
)

# For local computation (fast, stable)
s_configure(
  batch_size = 500,     # Reduce I/O overhead
  retry_attempts = 1    # Errors are usually persistent
)

# For development/debugging
s_configure(
  batch_size = 10,      # Easy to test recovery
  retry_attempts = 1,   # Fail fast
  auto_recover = FALSE  # Start fresh each run
)

# Reset to defaults
s_configure()

## -----------------------------------------------------------------------------
# Define example data and functions
data <- 1:20
algo_v1 <- function(x) x^2
algo_v2 <- function(x) x^3

# Scenario 1: Versioned computation
result_v1 <- s_map(data, algo_v1, .session_id = "analysis_v1")
result_v2 <- s_map(data, algo_v2, .session_id = "analysis_v2")

# Scenario 2: Named experiments
func <- function(x) x * 2
result_a <- s_map(data, func, .session_id = "experiment_baseline")
result_b <- s_map(data, func, .session_id = "experiment_treatment")

# Scenario 3: Date-based sessions
today <- format(Sys.Date(), "%Y%m%d")
result <- s_map(data, func, .session_id = paste0("daily_job_", today))

## -----------------------------------------------------------------------------
# Checkpoints are stored in R's user cache directory:
# - Linux:   ~/.cache/R/SafeMapper/checkpoints/
# - macOS:   ~/Library/Caches/org.R-project.R/R/SafeMapper/checkpoints/
# - Windows: %LOCALAPPDATA%/R/cache/R/SafeMapper/checkpoints/

# Each checkpoint is an .rds file named after its session_id

## -----------------------------------------------------------------------------
# Clean sessions older than 7 days (default)
s_clean_sessions(older_than_days = 7)

## ----eval=FALSE---------------------------------------------------------------
# # Clean by age
# s_clean_sessions(older_than_days = 30)  # Remove sessions > 30 days old
# 
# # Clean specific sessions
# s_clean_sessions(session_ids = c("old_experiment", "failed_job"))
# 
# # Clean by status
# s_clean_sessions(status_filter = "failed")     # Only failed sessions
# s_clean_sessions(status_filter = "corrupted")  # Only corrupted sessions

## ----eval=FALSE---------------------------------------------------------------
# # daily_job.R
# 
# library(SafeMapper)
# 
# # Configure for production
# s_configure(
#   batch_size = 100,
#   retry_attempts = 3
# )
# 
# # Use date-based session ID for predictable behavior
# job_id <- paste0("daily_process_", format(Sys.Date(), "%Y%m%d"))
# 
# # Run the job
# results <- s_map(
#   large_dataset,
#   process_record,
#   .session_id = job_id
# )
# 
# # Clean up old sessions at the end
# s_clean_sessions(older_than_days = 7)

## ----eval=FALSE---------------------------------------------------------------
# # development.R
# 
# library(SafeMapper)
# 
# # Configure for debugging
# s_configure(
#   batch_size = 10,
#   retry_attempts = 1,
#   auto_recover = FALSE  # Always start fresh
# )
# 
# # Test with small dataset
# test_data <- head(full_data, 50)
# 
# # Run and iterate
# results <- s_map(test_data, my_function)
# 
# # When ready for production, change config
# s_configure(
#   batch_size = 100,
#   retry_attempts = 3,
#   auto_recover = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# # multi_stage_pipeline.R
# 
# library(SafeMapper)
# 
# # Stage 1: Data extraction
# stage1_results <- s_map(
#   sources,
#   extract_data,
#   .session_id = "pipeline_stage1_v1"
# )
# 
# # Stage 2: Transformation
# stage2_results <- s_map(
#   stage1_results,
#   transform_data,
#   .session_id = "pipeline_stage2_v1"
# )
# 
# # Stage 3: Loading
# s_walk(
#   stage2_results,
#   load_to_database,
#   .session_id = "pipeline_stage3_v1"
# )
# 
# # If pipeline changes, increment version
# # .session_id = "pipeline_stage1_v2"

## ----eval=FALSE---------------------------------------------------------------
# # Possible causes:
# # 1. auto_recover is FALSE
# s_configure(auto_recover = TRUE)
# 
# # 2. Data changed (different fingerprint)
# # Use explicit session_id to force same session
# result <- s_map(data, func, .session_id = "fixed_session")
# 
# # 3. Checkpoint was deleted
# # Check if file exists in checkpoint directory

## -----------------------------------------------------------------------------
# Aggressive cleanup
s_clean_sessions(older_than_days = 1)

## -----------------------------------------------------------------------------
# Clean corrupted checkpoints
s_clean_sessions(status_filter = "corrupted")

