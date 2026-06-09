#' RF_scorch_int.RDS Default random forest model for predicting crown scorch
#'
#' This `randomForest` model was trained on segmented longleaf pine trees collected
#' using a RIEGL vz400i terrestrial lidar scanner, following Cannon et al. (2025).
#' Users may train custom models for other species or instrumentation.
#' @name RF_scorch_int.RDS
#' @format A `randomForest` object saved as an `.RDS` file
#' @source Cannon, J., et al. 2025. [https://doi.org/10.1186/s42408-025-00420-0]
#' @examples
#' model_file <- system.file("extdata", "RF_scorch_int.RDS", package = "CrownScorchTLS")
#' model.RF <- readRDS(model_file)
#' print(model.RF)
