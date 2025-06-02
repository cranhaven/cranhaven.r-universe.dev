## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = 'center',
  fig.height = 5,
  fig.width = 5
)

## ----libs, message=FALSE------------------------------------------------------
library(dplyr)
library(spectacles)
library(caret)

## ----data---------------------------------------------------------------------
# This loads the "australia" example dataset
oz <- load_oz()

## ----splice-------------------------------------------------------------------
oz <- splice(oz)

## ----split--------------------------------------------------------------------
set.seed(1) # To make the split reproducible
idx <- sample(1:nrow(oz), size = 75)
oz_calib <- oz[idx, ]
oz_valid <- oz[-idx, ]

## ----extracts-----------------------------------------------------------------
# The `spectra` function extracts the spectral matrix...
spec_mat <- spectra(oz)
big.head(spec_mat)

# ... while analytical data can be accessed using `$`
oz$carbon

## ----fit_1--------------------------------------------------------------------
fit1 <- train(
  # The `spectra` function extract the spectra matrix
  x = spectra(oz_calib), 
  # analytical data can be extracted using `$`
  y = oz_calib$carbon,
  # Here we choose the PLS regression method
  method = "pls",
  # The train function will try 3 possible parameters for the PLS
  tuneLength = 3
)

## ----fit_2--------------------------------------------------------------------
fit2 <- train(
    x = spectra(oz_calib),
    y = oz_calib$carbon,
    method = "pls",
    tuneLength = 10,
    trControl = trainControl(
      # Here we can specify the summary function used during parametrisation
      summaryFunction = spectroSummary
    ),
    # Here we can specifiy which metric to use to optimise the model parameters
    metric = "RPIQ"
)

## ----summarySpectro-----------------------------------------------------------
plot(fit2)
print(fit2)

## ----2mods--------------------------------------------------------------------
preds <- extractPrediction(
  # Here we specify the `caret` models we want to compare
  models = list(
    pls1 = fit1, 
    pls2 = fit2
  ), 
  testX = spectra(oz_valid), 
  testY = oz_valid$carbon
) 

# necessary so 2 PLS model can be compared in `plotObsVsPred`
preds$model <- preds$object

plotObsVsPred(preds)

## ----postResampleSpectro1-----------------------------------------------------
# Simple example for the entire dataset
postResampleSpectro(
  pred = predict(fit2, spectra(oz)), 
  obs = oz_valid$carbon
)

## ----postResampleSpectro2-----------------------------------------------------
# Run model predictions and extract performance statistics for 
# caliration and validation
res_calibration <- postResampleSpectro(pred = predict(fit2, spectra(oz_calib)), obs = oz_calib$carbon)
res_validation <- postResampleSpectro(pred = predict(fit2, spectra(oz_valid)), obs = oz_valid$carbon)

# Bootstrapped results can be extracted from the `train` object:
res_boot <- fit2$results %>% 
  filter(ncomp == fit2$bestTune$ncomp) %>% 
  select(names(res_calibration))

# Assemble the calibration, validation, and 
# bootstrapped results in a single data.frame
res <- rbind(
  data.frame(type = "Calibration", t(res_calibration)),
  data.frame(type = "Validation", t(res_validation)),
  data.frame(type = "Bootstrap", res_boot)
)

## ----res_table, results='asis', echo=FALSE------------------------------------
knitr::kable(res, digits = 2)

