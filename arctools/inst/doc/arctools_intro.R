## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  echo = TRUE, 
  cache = FALSE, 
  message = FALSE
)

## -----------------------------------------------------------------------------
library(arctools)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

## Read one of the data sets
fpath <- system.file("extdata", extdata_fnames[1], package = "arctools")
dat   <- as.data.frame(fread(fpath))
rbind(head(dat, 3), tail(dat, 3))

## ---- fig.width=8, fig.height=3.5---------------------------------------------
## Plot activity counts
## Format timestamp data column from character to POSIXct object
ggplot(dat, aes(x = ymd_hms(timestamp), y = vectormagnitude)) + 
  geom_line(size = 0.3, alpha = 0.8) + 
  labs(x = "Time", y = "Activity counts") + 
  theme_gray(base_size = 10) + 
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d")

## -----------------------------------------------------------------------------
acc    <- dat$vectormagnitude
acc_ts <- ymd_hms(dat$timestamp)

activity_stats(acc, acc_ts)

## -----------------------------------------------------------------------------
subset_12am_6am <- 1 : (6 * 1440/24)
activity_stats(acc, acc_ts, subset_minutes = subset_12am_6am) 

## -----------------------------------------------------------------------------
subset_12am_6am = 1 : (6/24 * 1440)
subset_6am_12pm = (6/24 * 1440 + 1) : (12/24 * 1440) 
subset_12pm_6pm = (12/24 * 1440 + 1) : (18/24 * 1440) 
subset_6pm_12am = (18/24 * 1440 + 1) : (24/24 * 1440) 
out <- rbind(
  activity_stats(acc, acc_ts, subset_minutes = subset_12am_6am, adjust_out_colnames = FALSE),
  activity_stats(acc, acc_ts, subset_minutes = subset_6am_12pm, adjust_out_colnames = FALSE),
  activity_stats(acc, acc_ts, subset_minutes = subset_12pm_6pm, adjust_out_colnames = FALSE),
  activity_stats(acc, acc_ts, subset_minutes = subset_6pm_12am, adjust_out_colnames = FALSE))
rownames(out) <- c("12am-6am", "6am-12pm", "12pm-6pm", "6pm-12am")
out

## -----------------------------------------------------------------------------
# day of a week indices 2,3,4,5,6 correspond to Mon,Tue,Wed,Thu,Fri 
subset_weekdays <- c(2:6)
activity_stats(acc, acc_ts, subset_weekdays = subset_weekdays) 

## -----------------------------------------------------------------------------
# day of a week indices 7,1 correspond to Sat,Sun
subset_weekdays <- c(7,1)
activity_stats(acc, acc_ts, subset_weekdays = subset_weekdays, subset_minutes = subset_6am_12pm) 

## -----------------------------------------------------------------------------
subset_11pm_5am <- c(
  (23 * 1440/24 + 1) : 1440,   ## 11:00 PM - midnight
  1 : (5 * 1440/24)            ## midnight - 5:00 AM
) 
activity_stats(acc, acc_ts, exclude_minutes = subset_11pm_5am) 

## -----------------------------------------------------------------------------
## Read sleep details data file
SleepDetails_fname <- "BatchSleepExportDetails_2020-05-01_14-00-46.csv"
SleepDetails_fpath <- system.file("extdata", SleepDetails_fname, package = "arctools")
SleepDetails       <- as.data.frame(fread(SleepDetails_fpath))

## Filter sleep details data to keep ID1 file 
SleepDetails_sub <-
    SleepDetails %>%
    filter(`Subject Name` == "ID_1") %>%
    select(`Subject Name`, `In Bed Time`, `Out Bed Time`) 
str(SleepDetails_sub)

## -----------------------------------------------------------------------------
in_bed_time  <- mdy_hms(SleepDetails_sub[, "In Bed Time"])
out_bed_time <- mdy_hms(SleepDetails_sub[, "Out Bed Time"])

activity_stats(acc, acc_ts, in_bed_time = in_bed_time, out_bed_time = out_bed_time) 

## -----------------------------------------------------------------------------
df <- data.frame(acc = acc, acc_ts = acc_ts)
rbind(head(df, 3), tail(df, 3))

## -----------------------------------------------------------------------------
acc <- midnight_to_midnight(acc = acc, acc_ts = acc_ts)

## Vector length on non NA-obs, vector length after acc 
c(length(acc[!is.na(acc)]), length(acc))

## -----------------------------------------------------------------------------
wear_flag <- get_wear_flag(acc)

## Proportion of wear time across the days
wear_flag_mat <- matrix(wear_flag, ncol = 1440, byrow = TRUE)
round(apply(wear_flag_mat, 1, sum, na.rm = TRUE) / 1440, 3)

## -----------------------------------------------------------------------------
valid_day_flag <- get_valid_day_flag(wear_flag)

## Compute number of valid days
valid_day_flag_mat <- matrix(valid_day_flag, ncol = 1440, byrow = TRUE)
apply(valid_day_flag_mat, 1, mean, na.rm = TRUE)

## -----------------------------------------------------------------------------
## Copies of original objects for the purpose of demonstration
acc_cpy  <- acc
wear_flag_cpy <- wear_flag

## Artificially replace 1h (4%) of a valid day with non-wear 
repl_idx <- seq(from = 1441, by = 1, length.out = 60)
acc_cpy[repl_idx] <- 0
wear_flag_cpy[repl_idx] <- 0

## Impute data for minutes identified as non-wear in days identified as valid
acc_cpy_imputed <- impute_missing_data(acc_cpy, wear_flag_cpy, valid_day_flag)

## Compare mean activity count on valid days before and after imputation
c(mean(acc_cpy[which(valid_day_flag == 1)]), 
  mean(acc_cpy_imputed[which(valid_day_flag == 1)]))

## -----------------------------------------------------------------------------
summarize_PA(acc, acc_ts, wear_flag, valid_day_flag) 

## -----------------------------------------------------------------------------
activity_stats(dat$vectormagnitude, ymd_hms(dat$timestamp))

