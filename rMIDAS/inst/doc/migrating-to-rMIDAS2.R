## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # Remove rMIDAS (optional -- it can coexist)
# # remove.packages("rMIDAS")
# 
# # Install rMIDAS2
# install.packages("rMIDAS2")
# 
# # One-time Python backend setup
# library(rMIDAS2)
# install_backend()

## -----------------------------------------------------------------------------
# # --- rMIDAS ---
# library(rMIDAS)
# # Python environment configured automatically on first load,
# # or manually via set_python_env()

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# library(rMIDAS2)
# install_backend()        # one-time setup
# # The server starts automatically when you call any imputation function

## -----------------------------------------------------------------------------
# # --- rMIDAS ---
# data(adult)
# adult_conv <- convert(adult,
#                       bin_cols = c("income"),
#                       cat_cols = c("workclass", "marital_status"),
#                       minmax_scale = TRUE)

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# # No convert() step needed. Pass raw data to midas() or midas_fit().

## -----------------------------------------------------------------------------
# # --- rMIDAS ---
# mid <- train(adult_conv,
#              training_epochs = 20L,
#              layer_structure = c(256, 256, 256),
#              input_drop      = 0.8,
#              learn_rate      = 0.0004,
#              seed            = 89L)

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# fit <- midas_fit(adult,
#                  epochs        = 20L,
#                  hidden_layers = c(256L, 128L, 64L),
#                  corrupt_rate  = 0.8,
#                  lr            = 0.001,
#                  seed          = 89L)

## -----------------------------------------------------------------------------
# # --- rMIDAS ---
# imps <- complete(mid, m = 10)
# # Returns a list of 10 data.frames
# head(imps[[1]])

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# imps <- midas_transform(fit, m = 10)
# # Returns a list of 10 data.frames
# head(imps[[1]])

## -----------------------------------------------------------------------------
# # --- rMIDAS2 (all-in-one) ---
# result <- midas(adult, m = 10, epochs = 20)
# head(result$imputations[[1]])

## -----------------------------------------------------------------------------
# # --- rMIDAS ---
# combine("income ~ age + hours_per_week", imps)

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# combine(fit, y = "income")
# 
# # Specify predictors explicitly:
# combine(fit, y = "income", ind_vars = c("age", "hours_per_week"))

## -----------------------------------------------------------------------------
# # --- rMIDAS ---
# overimpute(adult,
#            binary_columns  = c("income"),
#            softmax_columns = c("workclass", "marital_status"),
#            training_epochs = 20L,
#            spikein = 0.3)

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# diag <- overimpute(fit, mask_frac = 0.1)
# diag$mean_rmse
# diag$rmse     # per-column RMSE

## -----------------------------------------------------------------------------
# # --- rMIDAS2 only ---
# mean_df <- imp_mean(fit)
# head(mean_df)

## -----------------------------------------------------------------------------
# # --- rMIDAS2 ---
# stop_server()

## -----------------------------------------------------------------------------
# library(rMIDAS)
# 
# data(adult)
# adult <- adult[1:1000, ]
# 
# # 1. Preprocess
# adult_conv <- convert(adult,
#                       bin_cols  = c("income"),
#                       cat_cols  = c("workclass", "marital_status"),
#                       minmax_scale = TRUE)
# 
# # 2. Train
# mid <- train(adult_conv, training_epochs = 20L, seed = 89L)
# 
# # 3. Generate imputations
# imps <- complete(mid, m = 5)
# 
# # 4. Analyse
# combine("income ~ age + hours_per_week", imps)

## -----------------------------------------------------------------------------
# library(rMIDAS2)
# 
# data(adult)
# adult <- adult[1:1000, ]
# 
# # 1. Fit and impute (no preprocessing needed)
# result <- midas(adult, m = 5, epochs = 20, seed = 89L)
# 
# # 2. Analyse
# combine(result, y = "income", ind_vars = c("age", "hours_per_week"))
# 
# # 3. Clean up
# stop_server()

