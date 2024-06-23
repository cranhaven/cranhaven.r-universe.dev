## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(autocart)

## ----dataset------------------------------------------------------------------
# Load the example snow dataset provided by the autocart package
snow <- read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart"))

## -----------------------------------------------------------------------------
head(snow)

## -----------------------------------------------------------------------------
snow <- na.omit(snow)

## -----------------------------------------------------------------------------
# Extract the response vector in the regression tree
response <- as.matrix(snow$yr50)

# Create a dataframe for the predictors used in the model
predictors <- data.frame(snow$LONGITUDE, snow$LATITUDE, snow$ELEVATION, snow$YRS, snow$HUC,
                         snow$TD, snow$FFP, snow$MCMT, snow$MWMT, snow$PPTWT, snow$RH, snow$MAT)

# Create the matrix of locations so that autocart knows where our observations are located
locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))

# Split the data into 85% training data and 15% test data
numtraining <- round(0.85 * nrow(snow))
training_index <- rep(FALSE, nrow(snow))
training_index[1:numtraining] <- TRUE
training_index <- sample(training_index)

train_response <- response[training_index]
test_response <- response[!training_index]
train_predictors <- predictors[training_index, ]
test_predictors <- predictors[!training_index, ]
train_locations <- locations[training_index, ]
test_locations <- locations[!training_index, ]

## -----------------------------------------------------------------------------
alpha <- 0.60

## -----------------------------------------------------------------------------
beta <- 0.20

## -----------------------------------------------------------------------------
my_control <- autocartControl(distpower = 2)

## -----------------------------------------------------------------------------
snow_model <- autocart(train_response, train_predictors, train_locations, alpha, beta, my_control)

## -----------------------------------------------------------------------------
test_predictions <- predictAutocart(snow_model, test_predictors) 

## -----------------------------------------------------------------------------
residuals <- test_response - test_predictions

# RMSE
sqrt(mean(residuals^2))

