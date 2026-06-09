## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#remotes::install.packages('tiagodc/TreeLS')

## ----load-read, fig.width=4, fig.height=4-------------------------------------
library(CrownScorchTLS)
library(lidR)
# Download external file to a temporary .las/.laz file from data repo
url = "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/D-03-10867_post.laz"
las_file <- tempfile(fileext = paste0(".", tools::file_ext(url)))
download.file(url, las_file, mode = "wb", quiet = TRUE)
las <- readLAS(las_file)
#plot(las, color = 'Intensity')

## ----histogram, fig.width=4, fig.height=4, eval=FALSE-------------------------
# crown <- remove_stem(las)
# crown <- add_reflectance(crown)
# histogram <- get_histogram(crown)
# plot(density ~ intensity, data = histogram, xlab='Reflectance (dB)', type='l')

## ----predict, eval=FALSE------------------------------------------------------
# scorch <- predict_scorch(las)
# scorch

## ----multiple-trees, eval=FALSE, fig.width=4, fig.height=6--------------------
# directory <- system.file('extdata', package = 'CrownScorchTLS')
# filenames <- list.files(directory, pattern='.laz', full.names=TRUE)
# 
# par(mfrow = c(3,2), mar = c(4,4,1,1))
# for(f in filenames) {
#   las <- readLAS(f)
#   scorch <- suppressMessages(predict_scorch(las, plot=TRUE))
#   cat('file:\t', basename(f), '\t', 'scorch:\t', round(scorch,3),'\n')
# }

## ----custom-model, fig.width=4, fig.height=4, eval=FALSE----------------------
# ## ================================
# ## 1. Load segmented trees
# ## ================================
# 
# # --- Option A: Use example data from GitHub (default for vignette) ---
# urls <- c(
#   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/M-04-15549_post.laz",
#   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/L-05-14669_post.laz",
#   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/E-08-9269_post.laz",
#   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/B-04-4286_post.laz",
#   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/D-03-10867_post.laz",
#   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/C-04-11029_post.laz"
# )
# 
# # Download each file to a temporary .las/.laz file
# filenames <- sapply(urls, function(u) {
#   tf <- tempfile(fileext = paste0(".", tools::file_ext(u)))
#   download.file(u, tf, mode = "wb", quiet = TRUE)
#   tf
# })
# 
# 
# # --- Option B: User supplies their own local files ---
# # directory <- "C:/mydata/segmented_trees/"
# # filenames <- list.files(directory, pattern = "\\.(las|laz)$", full.names = TRUE)
# 
# 
# ## ================================
# ## 2. Extract histogram features
# ## ================================
# 
# library(tidyr)
# library(dplyr)
# 
# prediction_data <- lapply(filenames, function(f) {
#   las  <- readLAS(f)
#   crown <- tryCatch(remove_stem(las), error = function(e) las)
#   crown <- add_reflectance(crown)
# 
#   hist <- get_histogram(crown)
#   hist$intensity <- round(hist$intensity, 1)
# 
#   pivot_wider(hist,
#     names_from = intensity,
#     values_from = density,
#     names_prefix = "intensity_"
#   )
# })
# 
# prediction_data <- bind_rows(prediction_data)
# 
# 
# ## ================================
# ## 3. Train prediction model
# ## ================================
# 
# scorch_training <- c(0.05, 0.20, 0.50, 0.75, 0.95, 0.95)
# 
# library(randomForest)
# library(Boruta)
# 
# boruta.sel <- Boruta(x = prediction_data, y = scorch_training)
# important <- names(boruta.sel$finalDecision[boruta.sel$finalDecision != "Rejected"])
# 
# model.RF <- randomForest(
#   x = prediction_data[, important],
#   y = scorch_training
# )
# 
# print(model.RF)
# varImpPlot(model.RF)
# 
# 
# ## ================================
# ## 4. Predict scorch on new trees
# ## ================================
# 
# # --- User-supplied new lidar object ---
# # new_las <- readLAS("C:/mydata/newtree1.las")
# 
# # Example using the first file:
# new_las <- readLAS(filenames[1])
# 
# predicted_scorch <- predict_scorch(new_las, model = model.RF)
# predicted_scorch
# 

