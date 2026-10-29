## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    eval = TRUE,
    fig.align = "center",
    fig.width = 6,
    fig.height = 4
)
library(Epoch)

## ----list_projects------------------------------------------------------------
EpochRepos()

## ----downloader_predefined----------------------------------------------------
downloader <- EpochDownloader(progress=FALSE)
downloader

## ----explore_data-------------------------------------------------------------
names(downloader)

## ----load_single--------------------------------------------------------------
# Load the first available dataset
epoch_data <- downloader$FragilityData_subpt01_1

epoch_data

## ----load_multiple------------------------------------------------------------
# Load two datasets
selected_data <- c("FragilityData_subpt01_1", "FragilityData_subpt01_2")
epoch_list <- downloader[selected_data]

epoch_list

## ----eval=FALSE---------------------------------------------------------------
# wiki(downloader)

## ----epoch_structure----------------------------------------------------------
# Assuming we have an epoch object from previous steps
epoch <- epoch_data  # or epoch_list[[1]] if you loaded multiple

print(epoch) 

## -----------------------------------------------------------------------------
plot(epoch)

## -----------------------------------------------------------------------------
# Basic information
dim(epoch)  # dimensions: electrodes x time points
nrow(epoch)  # number of electrodes
ncol(epoch)  # number of time points
rownames(epoch)  # electrode names
range(coltimes(epoch))  # time points range (colnames work but will return character vector)

# Access the underlying data
data_matrix <- tblData(epoch)
data_matrix[1:5, 1:5]

## ----metadata-----------------------------------------------------------------
# Access row metadata (electrode information)
electrode_info <- rowData(epoch)
print(electrode_info)

# Access column metadata (there is no column metadata in this example)
time_info <- colData(epoch)
print(time_info)

# Access general metadata
meta_info <- metaData(epoch)
str(meta_info)

## -----------------------------------------------------------------------------
epoch[1:3, ]

## -----------------------------------------------------------------------------
electrode_names <- rownames(epoch)[1:3]
epoch[electrode_names, ]

## -----------------------------------------------------------------------------
epoch[electrode_names]

## -----------------------------------------------------------------------------
epoch[, 1:100]

## -----------------------------------------------------------------------------
crop(epoch, start = -0.5, end = 1.5)

## ----resample_epoch-----------------------------------------------------------
# Resample to 250 Hz
resampled_epoch <- resample(epoch, samplingRate = 250)
resampled_epoch

## ----create_epoch-------------------------------------------------------------
# Create sample iEEG-like data
n_electrodes <- 10
n_timepoints <- 1000
sampling_rate <- 500  # Hz
start_time <- -0.5    # seconds

# Generate synthetic data
synthetic_data <- matrix(
  rnorm(n_electrodes * n_timepoints), 
  nrow = n_electrodes, 
  ncol = n_timepoints
)

# Create electrode names
electrode_names <- paste0("Electrode_", sprintf("%02d", 1:n_electrodes))

## ----add_metadata-------------------------------------------------------------
# Create electrode metadata
electrode_metadata <- data.frame(
  electrode_id = electrode_names,
  brain_region = rep(c("Frontal", "Temporal", "Parietal"), length.out = n_electrodes),
  hemisphere = rep(c("Left", "Right"), length.out = n_electrodes),
  depth = runif(n_electrodes, 10, 50),  # depth in mm
  stringsAsFactors = FALSE
)

# Create time metadata
time_points <- seq(start_time, by = 1/sampling_rate, length.out = n_timepoints)
time_metadata <- data.frame(
  epoch_phase = ifelse(time_points < 0, "pre_stimulus", "post_stimulus"),
  stringsAsFactors = FALSE
)

# Create general metadata
general_metadata <- list(
  subject_id = "SUB001",
  session = "Session1",
  task = "Memory Task",
  sampling_rate = sampling_rate,
  recording_date = Sys.Date()
)


## -----------------------------------------------------------------------------
comprehensive_epoch <- Epoch(
  table = synthetic_data,
  electrodes = electrode_names,
  startTime = start_time,
  samplingRate = sampling_rate,
  rowData = electrode_metadata,
  colData = time_metadata,
  metaData = general_metadata
)

comprehensive_epoch

