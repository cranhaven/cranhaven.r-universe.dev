# Create internal environment
.arutools <- rlang::new_environment(parent = rlang::empty_env())

## Pattern type patterns for
.arutools$pattern_aru_type <- c(
  "barlt" = "BAR-LT",
  "SMM" = "Song Meter Mini",
  "SM(\\d)" = "Song Meter \\1",
  "S(\\d)A" = "Song Meter \\1"
)

## Variables for clean_logs()
# # Pattern to id the correct kind of log file
.arutools$pattern_check <- "FRONTIER LABS Bioacoustic Audio Recorder"

# Pattern to match the sunrise schedule in the log files.
.arutools$pattern_sr <- "(SR)"

# Pattern to match the sunset schedule in the log files.
.arutools$pattern_ss <- "(SS)"

# # Define data to extract and patterns
.arutools$pattern_data <- list(
  meta_serial = "Serial Number: ",
  meta_firmware = "Firmware: ",
  schedule_name = "Name: ",
  schedule_gps = "Backup GPS location: ",
  schedule_sr = glue::glue(" +\\d{{1,2}}\\) \\\"{.arutools$pattern_sr}\\\""),
  schedule_ss = glue::glue(" +\\d{{1,2}}\\) \\\"{.arutools$pattern_ss}\\\""),
  gps_position = "GPS position lock acquired \\[",
  recordings = "((Recording stopped. )|(\\| New recording started: ))"
  # pat_collapse(c(
  #   "start" = "\\| New recording started: ",
  #   "end" = "Recording stopped. "
  # ))
)

# Because log files suck and can sometimes have *both* dmy AND ymd formats in
# # the same file.
.arutools$pattern_date_time <-
  "(((((\\d{2})(\\/)(\\d{2})(\\/)((([12]{1}\\d{3}))))))|(((((([12]{1}\\d{3})))(_|-|)(\\d{2})(_|-|)(\\d{2}))))) ([0-2]{1}[0-9]{1})(_|-|:|)([0-5]{1}[0-9]{1})((_|-|:|)([0-5]{1}[0-9]{1}))"

