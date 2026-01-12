## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7.15, fig.height = 4)

## ---- warning = FALSE, message = FALSE----------------------------------------
library(forecastML)
library(DT)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]
DT::datatable(head(data, 5))

## -----------------------------------------------------------------------------
date_frequency <- "1 month"

dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)

## -----------------------------------------------------------------------------
horizons <- c(1, 6, 12)  # forecasting 1, 1:6, and 1:12 months into the future.

# Create a list of length 3, one slot for each modeled forecast horizon.
lookback_control <- vector("list", length(horizons))

# Within each horizon-specific list, we'll identify the custom feature lags.
lookback_control <- lapply(lookback_control, function(x) {
  list(
    c(3, 12),  # column 1: DriversKilled
    1:3,       # column 2: kms
    1:12,      # column 3: PetrolPrice
    0          # column 4: law; this could be any value, dynamic features are set to '0' internally.
    )
  })

data_train <- forecastML::create_lagged_df(data, type = "train",
                                           outcome_col = 1,
                                           horizons = horizons, 
                                           lookback_control = lookback_control,
                                           dates = dates,
                                           frequency = date_frequency,
                                           dynamic_features = "law")

## ---- results = 'hide'--------------------------------------------------------
plot(data_train)

## -----------------------------------------------------------------------------
horizons <- c(1, 6, 12)  # forecasting 1, 1:6, and 1:12 months into the future.

# A list of length 3, one slot for each modeled forecast horizon.
lookback_control <- vector("list", length(horizons))
lookback_control <- lapply(lookback_control, function(x) {
  
  # 12 feature lags for each of our 4 modeled features. Dynamic features will be coerced to "0" internally.
  lapply(1:4, function(x) {1:12})
  })

# Find the column index of the feature that we're removing.
remove_col <- which(grepl("PetrolPrice", names(data)))

# Remove the feature from the 12-month-out lagged data.frame.
lookback_control[[which(horizons == 12)]][remove_col] <- list(NULL)

data_train <- forecastML::create_lagged_df(data, type = "train",
                                           outcome_col = 1, 
                                           lookback_control = lookback_control,
                                           horizons = horizons,
                                           dates = dates,
                                           frequency = date_frequency,
                                           dynamic_features = "law")

## ---- results = 'hide'--------------------------------------------------------
plot(data_train)[[remove_col]]  # we're selecting 1 of our 3 lagged feature-level plots.

## -----------------------------------------------------------------------------
DT::datatable(head(data_train$horizon_12))

