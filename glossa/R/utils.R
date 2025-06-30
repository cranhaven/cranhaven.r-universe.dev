#=========================================================#
# glossa helpers  ----
#=========================================================#

#' Get Covariate Names
#'
#' This function extracts the names of covariates from a ZIP file containing covariate layers.
#'
#' @param file_path Path to the ZIP file containing covariate layers.
#' @return A character vector containing the names of covariates.
#' @details This function extracts the names of covariates from a ZIP file containing covariate layers.
#'
#' @keywords internal
get_covariate_names <- function(file_path){
  # Extract contents of the zip file
  tmpdir <- tempdir()
  zip_contents <- utils::unzip(file_path, exdir = tmpdir)

  # Get unique covariate directories
  covariates <- basename(unique(dirname(zip_contents)))

  return(covariates)
}


#' Extract Non-NA Covariate Values
#'
#' This function extracts covariate values for species occurrences, excluding NA values.
#'
#' @param data A data frame containing species occurrence data with columns x/long (first column) and y/lat (second column).
#' @param covariate_layers A list of raster layers representing covariates.
#' @param predictor_variables Variables to select from all the layers.
#' @return A data frame containing species occurrence data with covariate values, excluding NA values.
#' @details This function extracts covariate values for each species occurrence location from the provided covariate layers. It returns a data frame containing species occurrence data with covariate values, excluding any NA values.
#'
#' @export
extract_noNA_cov_values <- function(data, covariate_layers, predictor_variables){
  # Extract value by year
  covariate_values <- apply(data, 1, function(x){
    terra::extract(covariate_layers[[x["timestamp"]]], t(matrix(x[c(1, 2)])))
  })
  covariate_values <- do.call(rbind, covariate_values)

  covariate_values <- cbind(data, covariate_values[, predictor_variables, drop = FALSE]) %>%
    tidyr::drop_na()

  return(covariate_values)
}

#' Create Geographic Coordinate Layers
#'
#' Generates raster layers for longitude and latitude from given raster data,
#' applies optional scaling, and restricts the output to a specified spatial mask.
#'
#' @param layers Raster or stack of raster layers to derive geographic extent and resolution.
#' @param study_area Spatial object for masking output layers.
#' @param scale_layers Logical indicating if scaling is applied. Default is FALSE.
#'
#' @return Raster stack with layers lon and lat.
#'
#' @export
create_coords_layer <- function(layers, study_area = NULL, scale_layers = FALSE){
  # Create layer with longitude and latitude values
  coords_layer <- terra::rast(terra::ext(layers), resolution = terra::res(layers))
  terra::crs(coords_layer) <- terra::crs(layers)
  df_longlat <- terra::crds(coords_layer, df = TRUE)

  # Create longitude raster
  raster_long <- terra::rast(cbind(df_longlat, df_longlat$x))
  raster_long <- terra::extend(raster_long, coords_layer)
  terra::crs(raster_long) <- terra::crs(coords_layer)

  # Create latitude raster
  raster_lat <- terra::rast(cbind(df_longlat, df_longlat$y))
  raster_lat <- terra::extend(raster_lat, coords_layer)
  terra::crs(raster_lat) <- terra::crs(coords_layer)

  # Optionally scale the longitude and latitude rasters
  if (scale_layers) {
    raster_long <- terra::scale(raster_long)
    raster_lat <- terra::scale(raster_lat)
  }

  # Apply a mask to combined layers
  coords_layer <- c(raster_long, raster_lat)
  if(!is.null(study_area)){
    coords_layer <- terra::mask(coords_layer, terra::vect(study_area))
  }

  return(coords_layer)
}


#' Generate cross-validation folds
#'
#' Creates cross-validation fold assignments for presence-absence or presence-only data,
#' supporting three types of strategies: k-fold, spatial blocks (through blockCV R package), and temporal blocks.
#'
#' @param data A `data.frame` with at least presence-absence data (`pa`), coordinates, and optionally a `timestamp`.
#' @param method The cross-validation strategy. One of: `"k-fold"`, `"spatial_blocks"`, `"temporal_blocks"`.
#' @param block_method For spatial blocks, how to determine block size. One of: `"residuals_autocorrelation"`, `"predictors_autocorrelation"`, `"manual"`.
#' @param block_size Numeric. Manual block size in meters (used if `block_method = "manual"`).
#' @param k Integer. Number of folds to generate.
#' @param predictor_raster A `terra::SpatRaster` used for estimating spatial autocorrelation (only needed if `block_method = "predictors_autocorrelation"`).
#' @param model_residuals A `data.frame` with residuals and coordinates (only needed if `block_method = "residuals_autocorrelation"`).
#' @param coords A character vector of length 2 indicating the longitude and latitude column names.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{folds}{A vector of fold assignments (one per row in `data`).}
#'   \item{method}{The CV method used.}
#'   \item{block_method}{The spatial block size method (if applicable).}
#'   \item{block_size}{The estimated or manual block size (in meters), if spatial blocks were used.}
#' }
#'
#' @export

generate_cv_folds <- function(
    data, method = "k-fold",
    block_method = "predictors_autocorrelation",
    block_size = NULL, k = 10,
    predictor_raster = NULL, model_residuals = NULL,
    coords = c("decimalLongitude", "decimalLatitude")
) {
  .fit_variogram <- utils::getFromNamespace(".fit_variogram", "blockCV")
  sf::sf_use_s2(TRUE)
  if (method == "k-fold") {
    n <- nrow(data)
    folds <- sample(rep(1:k, length.out = n))

  } else if (method == "spatial_blocks") {
    if (block_method == "residuals_autocorrelation") {
      if (!is.null(model_residuals)) {
        sf_resid <- sf::st_as_sf(model_residuals, coords = coords, crs = 4326)
        block_size <- tryCatch({
          .fit_variogram(1, xx = sf_resid, column = "residual", num_sample = 5000)$var_model[2, 3] * 1000 # range in meters
        }, error = function(e) {
          message("Error in .fit_variogram: ", e$message)
          return(NULL)
        }
        )
      } else{
        warning("No model_residuals provided for residuals_autocorrelation.")
      }

    } else if (block_method == "predictors_autocorrelation") {
      if (!is.null(predictor_raster)) {
        # Get all layers that are not factors
        predictor_raster <- predictor_raster[[!sapply(predictor_raster, terra::is.factor)]]
        if (is.null(predictor_raster)) {
          warning("predictors_autocorrelation cannot be used with categorical layers")
        }

        tryCatch({
          n_layers <- terra::nlyr(predictor_raster)
          samples <- terra::spatSample(predictor_raster[[1]], size = min(length(terra::cells(predictor_raster)), 5000), method = "regular", as.points = TRUE, na.rm = TRUE)
          x_obj <- sf::st_as_sf(samples)

          vario_list <- lapply(seq_len(n_layers), function(i) {
            .fit_variogram(i, rr = predictor_raster, xx = NULL, column = NULL, num_sample = min(length(terra::cells(predictor_raster)), 5000))
          })

          ranges <- sapply(vario_list, function(v) v$var_model[2, 3])
          block_size <- median(ranges, na.rm = TRUE)

          if (sf::st_is_longlat(x_obj)) {
            block_size <- block_size * 1000  # convert to meters
          }
        }, error = function(e) {
          message("Error in predictors_autocorrelation: ", e$message)
          block_size <- NULL
        })
      } else {
        warning("predictor_raster is required for predictors_autocorrelation method")
      }

    } else if (block_method == "manual") {
      if (is.null(block_size)) {
        warning("block_size is required for manual method")
      }
    } else {
      warning("Invalid block_method. Choose from 'residuals_autocorrelation', 'predictors_autocorrelation', or 'manual'.")
    }

    if (is.null(block_size)){
      warning("Could not calculate block size with the specified method. Dividing extent evenly based on number of folds.")

      ext <- terra::ext(predictor_raster)
      width <- ext[2] - ext[1]
      height <- ext[4] - ext[3]
      smaller_dim <- min(width, height)

      block_size <- (smaller_dim / k) * 111325 # degree to meters
    }

    # Create folds
    sf_data <- sf::st_as_sf(data, coords = coords, crs = 4326)
    folds <- tryCatch(
      suppressWarnings(blockCV::cv_spatial(
        x = sf_data,
        column = "pa",
        size = block_size,
        k = k,
        selection = "random",
        iteration = 50
      )),
      error = function(e) {
        message("Error in cv_spatial: ", e$message)
        return(NULL)
      }
    )

    if (is.null(folds)) {
      warning("Unable to create block cv folds. 'k' is bigger than the number of spatial blocks generated using the given block size.")
    } else{
      folds <- folds$folds_ids
    }
  } else if (method == "temporal_blocks") {
    if (!"timestamp" %in% colnames(data)) {
      stop("data must contain a 'timestamp' column for temporal blocks.")
    }

    timecode <- sort(unique(data$timestamp))

    if (length(timecode) < k) {
      stop("Not enough unique timestamps to create the requested number of folds 'k'.")
      folds <- NULL
    } else {
      timewindows <- cut(timecode, breaks = k, labels = FALSE)
      names(timewindows) <- timecode
      folds <- timewindows[as.character(data$timestamp)]
    }
  }
  sf::sf_use_s2(FALSE)
  return(list(folds = folds, method = method, block_method = block_method, block_size = block_size))
}


#=========================================================#
# cutoff functions----
# Functions obtained from https://github.com/selva86/InformationValue
#=========================================================#

#' Compute specificity and sensitivity
#'
#' @details This function was obtained from the InformationValue R package (\url{https://github.com/selva86/InformationValue}).
#' @param actuals The actual binary flags for the response variable. It can take a numeric vector containing values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.
#' @param predictedScores The prediction probability scores for each observation. If your classification model gives the 1/0 predictions, convert it to a numeric vector of 1's and 0's.
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return A list with two elements: fpr (false positive rate) and tpr (true positive rate).
#' @keywords internal
getFprTpr<- function(actuals, predictedScores, threshold=0.5){
  return(list(1-specificity(actuals=actuals, predictedScores=predictedScores, threshold=threshold),
              sensitivity(actuals=actuals, predictedScores=predictedScores, threshold=threshold)))
}

#' Calculate the specificity for a given logit model
#'
#' @details This function was obtained from the InformationValue R package (\url{https://github.com/selva86/InformationValue}).
#' @param actuals The actual binary flags for the response variable. It can take a numeric vector containing values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.
#' @param predictedScores The prediction probability scores for each observation. If your classification model gives the 1/0 predictions, convert it to a numeric vector of 1's and 0's.
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The specificity of the given binary response actuals and predicted probability scores, which is, the number of observations without the event AND predicted to not have the event divided by the number of observations without the event.
#' @keywords internal
specificity <- function(actuals, predictedScores, threshold=0.5){
  predicted_dir <- ifelse(predictedScores < threshold, 0, 1)
  actual_dir <- actuals
  no_without_and_predicted_to_not_have_event <- sum(actual_dir != 1 & predicted_dir != 1, na.rm=TRUE)
  no_without_event <- sum(actual_dir != 1, na.rm=TRUE)
  return(no_without_and_predicted_to_not_have_event/no_without_event)
}

#' Calculate the sensitivity for a given logit model
#'
#' @details This function was obtained from the InformationValue R package (\url{https://github.com/selva86/InformationValue}).
#' @param actuals The actual binary flags for the response variable. It can take a numeric vector containing values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.
#' @param predictedScores The prediction probability scores for each observation. If your classification model gives the 1/0 predictions, convert it to a numeric vector of 1's and 0's.
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The sensitivity of the given binary response actuals and predicted probability scores, which is, the number of observations with the event AND predicted to have the event divided by the number of observations with the event.
#' @keywords internal
sensitivity <- function(actuals, predictedScores, threshold=0.5){
  predicted_dir <- ifelse(predictedScores < threshold, 0, 1)
  actual_dir <- actuals
  no_with_and_predicted_to_have_event <- sum(actual_dir == 1 & predicted_dir == 1, na.rm=TRUE)
  no_with_event <- sum(actual_dir == 1, na.rm=TRUE)
  return(no_with_and_predicted_to_have_event/no_with_event)
}

#' Calculate Youden's index
#'
#' @details This function was obtained from the InformationValue R package (\url{https://github.com/selva86/InformationValue}).
#' @param actuals The actual binary flags for the response variable. It can take a numeric vector containing values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.
#' @param predictedScores The prediction probability scores for each observation. If your classification model gives the 1/0 predictions, convert it to a numeric vector of 1's and 0's.
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The youdensIndex of the given binary response actuals and predicted probability scores, which is calculated as Sensitivity + Specificity - 1
#' @keywords internal
youdensIndex <- function(actuals, predictedScores, threshold=0.5){
  Sensitivity <- sensitivity(actuals, predictedScores, threshold = threshold)
  Specificity <- specificity(actuals, predictedScores, threshold = threshold)
  return(Sensitivity + Specificity - 1)
}

#' Misclassification Error
#'
#' @details This function was obtained from the InformationValue R package (\url{https://github.com/selva86/InformationValue}).
#' @param actuals The actual binary flags for the response variable. It can take a numeric vector containing values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.
#' @param predictedScores The prediction probability scores for each observation. If your classification model gives the 1/0 predictions, convert it to a numeric vector of 1's and 0's.
#' @param threshold If predicted value is above the threshold, it will be considered as an event (1), else it will be a non-event (0). Defaults to 0.5.
#' @return The misclassification error, which tells what proportion of predicted direction did not match with the actuals.
#' @keywords internal
misClassError <- function(actuals, predictedScores, threshold=0.5){
  predicted_dir <- ifelse(predictedScores < threshold, 0, 1)
  actual_dir <- actuals
  return(round(sum(predicted_dir != actual_dir, na.rm=TRUE)/length(actual_dir), 4))
}

#' Compute the optimal probability cutoff score
#'
#' @details This function was obtained from the InformationValue R package (\url{https://github.com/selva86/InformationValue}).
#' @param actuals The actual binary flags for the response variable. It can take a numeric vector containing values of either 1 or 0, where 1 represents the 'Good' or 'Events' while 0 represents 'Bad' or 'Non-Events'.
#' @param predictedScores The prediction probability scores for each observation. If your classification model gives the 1/0 predictions, convert it to a numeric vector of 1's and 0's.
#' @param optimiseFor The maximization criterion for which probability cutoff score needs to be optimised. Can take either of following values: "Ones" or "Zeros" or "Both" or "misclasserror"(default). If "Ones" is used, 'optimalCutoff' will be chosen to maximise detection of "One's". If 'Both' is specified, the probability cut-off that gives maximum Youden's Index is chosen. If 'misclasserror' is specified, the probability cut-off that gives minimum mis-classification error is chosen.
#' @param returnDiagnostics If TRUE, would return additional diagnostics such as 'sensitivityTable', 'misclassificationError', 'TPR', 'FPR' and 'specificity' for the chosen cut-off.
#' @return The optimal probability score cutoff that maximises a given criterion. If 'returnDiagnostics' is TRUE, then the following items are returned in a list:
#' @keywords internal
optimalCutoff <- function(actuals, predictedScores, optimiseFor="misclasserror", returnDiagnostics=FALSE){
  # initialise the diagnostics dataframe to study the effect of various cutoff values.
  sequence <- seq(max(predictedScores), min(predictedScores), -0.01)
  sensMat <- data.frame(CUTOFF=sequence, FPR= numeric(length(sequence)),TPR= numeric(length(sequence)), YOUDENSINDEX=numeric(length(sequence)))
  sensMat[, c(2:3)] <- as.data.frame(t(mapply(getFprTpr, threshold=sequence, MoreArgs=list(actuals=actuals, predictedScores=predictedScores))))
  sensMat$YOUDENSINDEX <- mapply(youdensIndex, threshold=sequence, MoreArgs=list(actuals=actuals, predictedScores=predictedScores))
  sensMat$SPECIFICITY <- (1 - as.numeric(sensMat$FPR))
  sensMat$MISCLASSERROR <- mapply(misClassError, threshold=sequence, MoreArgs=list(actuals=actuals, predictedScores=predictedScores))

  # Select the cutoff
  if(optimiseFor=="Both"){
    rowIndex <- which(sensMat$YOUDENSINDEX == max(as.numeric(sensMat$YOUDENSINDEX)))[1]  # choose the maximum cutoff
  }else if(optimiseFor=="Ones"){
    rowIndex <- which(sensMat$TPR == max(as.numeric(sensMat$TPR)))[1]  # choose the maximum cutoff
  }else if(optimiseFor=="Zeros"){
    rowIndex <- tail(which(sensMat$SPECIFICITY == max(as.numeric(sensMat$SPECIFICITY))), 1)  # choose the minimum cutoff
  }else if(optimiseFor=="misclasserror"){
    rowIndex <- tail(which(sensMat$MISCLASSERROR == min(as.numeric(sensMat$MISCLASSERROR))), 1)  # choose the minimum cutoff
  }

  # what should the function return
  if(!returnDiagnostics){
    return(sensMat$CUTOFF[rowIndex])
  } else {
    output <- vector(length=6, mode="list")  # initialise diagnostics output
    names(output) <- c("optimalCutoff", "sensitivityTable", "misclassificationError", "TPR", "FPR", "Specificity")  # give names
    output$optimalCutoff <- sensMat$CUTOFF[rowIndex]
    output$sensitivityTable <- sensMat
    output$misclassificationError <- misClassError(actuals, predictedScores, threshold=sensMat$CUTOFF[rowIndex])
    output$TPR <- getFprTpr(actuals, predictedScores, threshold=sensMat$CUTOFF[rowIndex])[[2]]
    output$FPR <- getFprTpr(actuals, predictedScores, threshold=sensMat$CUTOFF[rowIndex])[[1]]
    output$Specificity <- sensMat$SPECIFICITY[rowIndex]
    return(output)
  }
}

#=========================================================#
# Evaluation metrics ----
#=========================================================#

#' Continuous Boyce Index (CBI) with weighting
#'
#' This function is a copy from the `contBoyce()` function from the `enmSdm` R package.
#' This function calculates the continuous Boyce index (CBI), a measure of model accuracy for presence-only test data. This version uses multiple, overlapping windows, in contrast to \code{link{contBoyce2x}}, which covers each point by at most two windows.
#' @param pres Numeric vector. Predicted values at presence sites.
#' @param contrast Numeric vector. Predicted values at background sites.
#' @param numBins Positive integer. Number of (overlapping) bins into which to divide predictions.
#' @param binWidth Positive numeric value < 1. Size of a bin. Each bin will be \code{binWidth * (max - min)}. If \code{autoWindow} is \code{FALSE} (the default) then \code{min} is 0 and \code{max} is 1. If \code{autoWindow} is \code{TRUE} then \code{min} and \code{max} are the maximum and minimum value of all predictions in the background and presence sets (i.e., not necessarily 0 and 1).
#' @param presWeight Numeric vector same length as \code{pres}. Relative weights of presence sites. The default is to assign each presence a weight of 1.
#' @param contrastWeight Numeric vector same length as \code{contrast}. Relative weights of background sites. The default is to assign each presence a weight of 1.
#' @param autoWindow Logical. If \code{FALSE} calculate bin boundaries starting at 0 and ending at 1 + epsilon (where epsilon is a very small number to assure inclusion of cases that equal 1 exactly). If \code{TRUE} (default) then calculate bin boundaries starting at minimum predicted value and ending at maximum predicted value.
#' @param method Character. Type of correlation to calculate. The default is \code{'spearman'}, the Spearman rank correlation coefficient used by Boyce et al. (2002) and Hirzel et al. (2006), which is the "traditional" CBI. In contrast, \code{'pearson'} or \code{'kendall'} can be used instead.  See \code{\link[stats]{cor}} for more details.
#' @param dropZeros Logical. If \code{TRUE} then drop all bins in which the frequency of presences is 0.
#' @param na.rm Logical. If \code{TRUE} then remove any presences and associated weights and background predictions and associated weights with \code{NA}s.
#' @param ... Other arguments (not used).
#' @return Numeric value.
#'
#' @details CBI is the Spearman rank correlation coefficient between the proportion of sites in each prediction class and the expected proportion of predictions in each prediction class based on the proportion of the landscape that is in that class.  The index ranges from -1 to 1. Values >0 indicate the model's output is positively correlated with the true probability of presence.  Values <0 indicate it is negatively correlated with the true probability of presence.
#'
#' @references Boyce, M.S., Vernier, P.R., Nielsen, S.E., and Schmiegelow, F.K.A.  2002.  Evaluating resource selection functions.  \emph{Ecological Modeling} 157:281-300. \doi{https://doi.org/10.1016/S0304-3800(02)00200-4}
#' @references Hirzel, A.H., Le Lay, G., Helfer, V., Randon, C., and Guisan, A.  2006.  Evaluating the ability of habitat suitability models to predict species presences.  \emph{Ecological Modeling} 199:142-152. \doi{10.1016/j.ecolmodel.2006.05.017}
#'
#' @note This function is directly copied from the `enmSdm` package.
#'
#' @export
contBoyce <- function(
    pres,
    contrast,
    presWeight = rep(1, length(pres)),
    contrastWeight = rep(1, length(contrast)),
    numBins = 101,
    binWidth = 0.1,
    autoWindow = TRUE,
    method = "spearman",
    dropZeros = TRUE,
    na.rm = FALSE,
    ...) {
  # if all NAs
  if (
    all(is.na(pres)) ||
    all(is.na(contrast)) ||
    all(is.na(presWeight)) ||
    all(is.na(contrastWeight))
  ) {
    return(NA)
  }

  # catch errors
  if (binWidth > 1 || binWidth <= 0) {
    stop('Argument "binWidth" must be between 0 and 1.')
  }

  eps <- .Machine$double.eps

  # right hand side of each class (assumes max value is >0)
  lowest <- if (autoWindow) {
    min(c(pres, contrast), na.rm = na.rm)
  } else {
    0
  }
  highest <- if (autoWindow) {
    max(c(pres, contrast), na.rm = na.rm) + eps
  } else {
    1 + eps
  }



  windowWidth <- binWidth * (highest - lowest)

  lows <- seq(lowest, highest - windowWidth, length.out = numBins)
  highs <- seq(lowest + windowWidth + eps, highest, length.out = numBins)

  ##########-
  ## MAIN ##
  ##########-

  ## initiate variables to store predicted/expected (P/E) values
  freqPres <- freqContrast <- rep(NA, length(numBins))

  ### tally proportion of test presences/background sites in each class
  for (countClass in 1:numBins) {
    # number of presence predictions in this class
    presInBin <- pres >= lows[countClass] & pres < highs[countClass]
    presInBin <- presInBin * presWeight
    freqPres[countClass] <- sum(presInBin, na.rm = na.rm)

    # number of background predictions in this class
    bgInBin <- contrast >= lows[countClass] & contrast < highs[countClass]
    bgInBin <- bgInBin * contrastWeight
    freqContrast[countClass] <- sum(bgInBin, na.rm = na.rm)
  } # next predicted value class

  # mean bin prediction
  meanPred <- rowMeans(cbind(lows, highs))

  # add small number to each bin that has 0 background frequency but does have a
  # presence frequency > 0
  if (any(freqPres > 0 & freqContrast == 0)) {
    smallValue <- min(0.5 * c(
      presWeight[presWeight > 0],
      contrastWeight[contrastWeight > 0]
    ))
    freqContrast[freqPres > 0 & freqContrast == 0] <- smallValue
  }

  # remove classes with 0 presence frequency
  if (dropZeros && 0 %in% freqPres) {
    zeros <- which(freqPres == 0)
    meanPred[zeros] <- NA
    freqPres[zeros] <- NA
    freqContrast[zeros] <- NA
  }

  # remove classes with 0 background frequency
  if (any(0 %in% freqContrast)) {
    zeros <- which(freqContrast == 0)
    meanPred[zeros] <- NA
    freqPres[zeros] <- NA
    freqContrast[zeros] <- NA
  }

  P <- freqPres / sum(presWeight, na.rm = TRUE)
  E <- freqContrast / sum(contrastWeight, na.rm = TRUE)
  PE <- P / E

  # remove NAs
  na_in_either <- (is.na(meanPred) | is.na(PE))
  meanPred <- meanPred[!na_in_either]
  PE <- PE[!na_in_either]

  # calculate continuous Boyce index (cbi)
  cbi <- stats::cor(x = meanPred, y = PE, method = method)
  cbi
}

#' Evaluation metrics for model predictions
#'
#' Computes a set of performance metrics (e.g., AUC, TSS, CBI) based on observed and predicted values.
#'
#' @param df A data.frame with columns: `observed` (0/1), `predicted` (0/1), `probability` (numeric).
#' @param na.rm Logical. Whether to remove rows with NA values.
#' @param method Correlation method for CBI ("spearman", "pearson", or "kendall").
#'
#' @return A named list or data.frame with evaluation metrics.
#' @export
evaluation_metrics <- function(df, na.rm = TRUE, method = "spearman") {
  required_cols <- c("observed", "predicted", "probability")
  if (!all(required_cols %in% colnames(df))) {
    stop("Input data.frame must contain: observed, predicted, and probability columns")
  }

  if (na.rm) df <- df[complete.cases(df[, required_cols]), ]

  y_true <- df$observed
  y_pred <- df$predicted
  y_prob <- df$probability

  # Confusion matrix components
  TP <- sum(y_true == 1 & y_pred == 1)
  FP <- sum(y_true == 0 & y_pred == 1)
  TN <- sum(y_true == 0 & y_pred == 0)
  FN <- sum(y_true == 1 & y_pred == 0)

  # in case of zeros
  safe_divide <- function(a, b) ifelse(b == 0, NA, a / b)

  PREC <- safe_divide(TP, TP + FP)
  SEN  <- safe_divide(TP, TP + FN)
  SPC  <- safe_divide(TN, TN + FP)
  FDR  <- safe_divide(FP, TP + FP)
  NPV  <- safe_divide(TN, FN + TN)
  FNR  <- safe_divide(FN, TP + FN)
  FPR  <- safe_divide(FP, FP + TN)
  Fscore <- 2 * safe_divide(PREC * SEN, PREC + SEN)
  ACC  <- safe_divide(TP + TN, TP + FP + FN + TN)
  BA   <- safe_divide(SEN + SPC, 2)
  TSS  <- SEN + SPC - 1

  # AUC
  AUC <- tryCatch({
    as.numeric(pROC::auc(pROC::roc(y_true, y_prob, levels = c(0, 1), direction = "<")))
  }, error = function(e) NA)

  # CBI
  CBI <- tryCatch({
    contBoyce(pres = y_prob[y_true == 1], contrast = y_prob[y_true == 0], method = method)
  }, error = function(e) NA)

  metrics <- c(
    TP = TP, FP = FP, FN = FN, TN = TN,
    PREC = PREC, SEN = SEN, SPC = SPC, FDR = FDR, NPV = NPV, FNR = FNR, FPR = FPR,
    Fscore = Fscore, ACC = ACC, BA = BA, TSS = TSS,
    CBI = CBI, AUC = AUC
  )

  return(metrics)
}


#=========================================================#
# shiny utils ----
#=========================================================#
# nocov start

#' Export Glossa Model Results
#'
#' This function exports various types of Glossa model results, including native range predictions, suitable habitat predictions, model data, variable importance, functional response results, and presence/absence probability cutoffs. It generates raster files for prediction results, TSV files for model data and variable importance, and TSV files for functional response results. Additionally, it creates a TSV file for presence/absence probability cutoffs if provided.
#'
#' @param species A character vector specifying the species names.
#' @param models A character vector specifying the types of models to export results for.
#' @param layer_results A list containing layer results for native range and suitable habitat predictions.
#' @param fields A character vector specifying the fields to include in the exported results.
#' @param model_data Logical, indicating whether to export model data.
#' @param fr Logical, indicating whether to export functional response results.
#' @param prob_cut Logical, indicating whether to export presence/absence probability cutoffs.
#' @param varimp Logical, indicating whether to export variable importance.
#' @param cross_val Logical, indicating whether to export cross-validation metrics.
#' @param layer_format A character vector specifying the format of the exported raster files.
#' @param projections_results A list containing projections results.
#' @param presence_absence_list A list containing presence/absence lists.
#' @param other_results A list containing other types of results (e.g., variable importance, functional responses, cross-validation).
#' @param pa_cutoff A list containing presence/absence probability cutoffs.
#'
#' @return A character vector of file paths for the exported files or directories.
#' @keywords internal
glossa_export <- function(species = NULL, models = NULL, layer_results = NULL, fields = NULL,
                          model_data = FALSE, model_summary = FALSE, fr = FALSE, prob_cut = FALSE,
                          varimp = FALSE, cross_val = FALSE, layer_format = "tif",
                          projections_results = NULL, presence_absence_list = NULL,
                          other_results = NULL, pa_cutoff = NULL, config_snapshot = NULL) {
  # Initialize an empty vector to store file paths of exported files
  export_files <- c()

  for (sp in species){
    tmp_sp <- file.path(tempdir(), sp)
    if (file.exists(tmp_sp)){
      unlink(tmp_sp, recursive = TRUE)
    }
    dir.create(tmp_sp)

    if ("native_range" %in% models){
      # Create a temporary directory to store native range files
      tmp_nr <- file.path(tmp_sp, "nr")
      dir.create(tmp_nr)

      # Iterate over each layer_results
      for (t in layer_results){
        if (t == "fit_layers"){
          tryCatch({
            dir.create(file.path(tmp_nr, "fit"))
            # Iterate over each field and export raster files
            for (value in fields) {
              dir.create(file.path(tmp_nr, "fit", value))
              terra::writeRaster(
                projections_results[[t]][["native_range"]][[sp]][[value]],
                filename = file.path(file.path(tmp_nr, "fit", value, paste(gsub(" ", "_", sp), "_nr_fit_", value, ".", layer_format, sep = ""))),
                overwrite = TRUE
              )
            }
          }, error = function(e) {
            warning("Native range fit_layers export failed for ", sp, ":", e$message)
          })
        } else if (t == "projections"){
          tryCatch({
            dir.create(file.path(tmp_nr, "proj"))
            # Iterate over each scenario, year, and field to export raster files
            for (scenario in names(projections_results[[t]][["native_range"]][[sp]])){
              dir.create(file.path(tmp_nr, "proj", scenario))
              for (value in fields) {
                dir.create(file.path(tmp_nr, "proj", scenario, value))
                for (year in seq_along(projections_results[[t]][["native_range"]][[sp]][[scenario]])){
                  terra::writeRaster(
                    projections_results[[t]][["native_range"]][[sp]][[scenario]][[year]][[value]],
                    filename = file.path(file.path(tmp_nr, "proj", scenario, value, paste(gsub(" ", "_", sp), "_nr_proj_", year, "_", value, ".", layer_format, sep = ""))),
                    overwrite = TRUE
                  )
                }
              }
            }
          }, error = function(e) {
            warning("Native range projections export failed for ", sp, ": ", e$message)
          })
        }
      }
    }


    if ("suitable_habitat" %in% models){
      # Create a temporary directory to store suitable habitat files
      tmp_sh <- file.path(tmp_sp, "sh")
      dir.create(tmp_sh)

      for (t in layer_results){
        if (t == "fit_layers"){
          tryCatch({
            dir.create(file.path(tmp_sh, "fit"))
            # Iterate over each field and export raster files
            for (value in fields) {
              dir.create(file.path(tmp_sh, "fit", value))
              terra::writeRaster(
                projections_results[[t]][["suitable_habitat"]][[sp]][[value]],
                filename = file.path(file.path(tmp_sh, "fit", value, paste(gsub(" ", "_", sp), "_sh_fit_", value, ".", layer_format, sep = ""))),
                overwrite = TRUE
              )
            }
          }, error = function(e) {
            warning("Suitable habitat fit_layers export failed for ", sp, ":", e$message)
          })
        } else if (t == "projections"){
          tryCatch({
            dir.create(file.path(tmp_sh, "proj"))
            # Iterate over each scenario, year, and field to export raster files
            for (scenario in names(projections_results[[t]][["suitable_habitat"]][[sp]])){
              dir.create(file.path(tmp_sh, "proj", scenario))
              for (value in fields) {
                dir.create(file.path(tmp_sh, "proj", scenario, value))
                for (year in seq_along(projections_results[[t]][["suitable_habitat"]][[sp]][[scenario]])){
                  terra::writeRaster(
                    projections_results[[t]][["suitable_habitat"]][[sp]][[scenario]][[year]][[value]],
                    filename = file.path(file.path(tmp_sh, "proj", scenario, value, paste(gsub(" ", "_", sp), "_sh_proj_", year, "_", value, ".", layer_format, sep = ""))),
                    overwrite = TRUE
                  )
                }
              }
            }
          }, error = function(e) {
            warning("Suitable habitat projections export failed for ", sp, ": ", e$message)
          })
        }
      }
    }

    # Export model data if requested
    if (model_data){
      tryCatch({
        if (!is.null(presence_absence_list[["model_pa"]])){
          tmp_model_data <- file.path(tmp_sp, paste(gsub(" ", "_", sp), "_model_data.tsv", sep = ""))
          df <- presence_absence_list[["model_pa"]][[sp]]
          write.table(df, tmp_model_data, quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
        } else {
          warning(paste("Unable to download model data for", sp, "as it has not been computed."))
        }
      }, error = function(e) {
        warning("Model data export failed for ", sp, ": ", e$message)
      })
    }

    # Export model summary if requested
    if (model_summary){
      tryCatch({
        if (!is.null(other_results[["model_diagnostic"]])){
          tmp_model_summary <- file.path(tmp_sp, "mod_diag")
          dir.create(tmp_model_summary)
          for (model_type in names(other_results[["model_diagnostic"]])){
            write.table(other_results[["model_diagnostic"]][[model_type]][[sp]][["data"]], file = file.path(tmp_model_summary, paste0(gsub(" ", "_", sp), "_", switch(model_type, "suitable_habitat" = "sh", "native_range" = "nr"), "_fitted_predictions.tsv")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
            mod_metrics <- other_results[["model_diagnostic"]][[model_type]][[sp]][["metrics"]]
            write.table(data.frame(metric = names(mod_metrics), value = mod_metrics), file = file.path(tmp_model_summary, paste0(gsub(" ", "_", sp), "_", switch(model_type, "suitable_habitat" = "sh", "native_range" = "nr"), "_fitted_metrics.tsv")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
          }
        } else {
          warning(paste("Unable to download confusion matrix for", sp, "as they have not been computed."))
        }
      }, error = function(e) {
        warning("Model summary export failed for ", sp, ": ", e$message)
      })
    }

    # Export variable importance if requested
    if (varimp){
      tryCatch({
        if (!is.null(other_results[["variable_importance"]])){
          tmp_var_imp <- file.path(tmp_sp, "var_imp")
          dir.create(tmp_var_imp)
          for (model_type in names(other_results[["variable_importance"]])){
            df <- other_results[["variable_importance"]][[model_type]][[sp]]
            write.table(df, file = file.path(tmp_var_imp, paste(gsub(" ", "_", sp), "_var_imp_", switch(model_type, "suitable_habitat" = "sh", "native_range" = "nr"), ".tsv", sep = "")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
          }
        } else {
          warning(paste("Unable to download variable importance for", sp, "as it has not been computed."))
        }
      }, error = function(e) {
        warning("Variable importance export failed for ", sp, ": ", e$message)
      })
    }

    # Export functional responses if requested
    if (fr){
      tryCatch({
        if (!is.null(other_results[["response_curve"]])){
          tmp_fr <- file.path(tmp_sp, "func_res")
          dir.create(tmp_fr)
          for (cov in names(other_results[["response_curve"]][[sp]])){
            write.table(other_results[["response_curve"]][[sp]][[cov]], file = file.path(tmp_fr, paste(gsub(" ", "_", sp), "_func_res_", cov, ".tsv", sep = "")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
          }
        } else {
          warning(paste("Unable to download functional response results for", sp, "as they have not been computed."))
        }
      }, error = function(e) {
        warning("Functional responses export failed for ", sp, ": ", e$message)
      })
    }

    # Export cross-validation metrics if requested
    if (cross_val){
      tryCatch({
        if (!is.null(other_results[["cross_validation"]])){
          tmp_cv <- file.path(tmp_sp, "cross_val")
          dir.create(tmp_cv)

          for (model_type in names(other_results[["cross_validation"]])) {
            if (!sp %in% names(other_results[["cross_validation"]][[model_type]])) {
              next
            }

            for (method in names(other_results[["cross_validation"]][[model_type]][[sp]])) {
              cv_result <- other_results[["cross_validation"]][[model_type]][[sp]][[method]]

              if (!is.null(cv_result$metrics)) {
                write.table(cv_result$metrics, file = file.path(tmp_cv, paste0(gsub(" ", "_", sp), "_cv_metrics_", switch(model_type, "suitable_habitat" = "sh", "native_range" = "nr"), "_", switch(method, "k-fold" = "k_fold", "spatial_blocks" = "sb", "temporal_blocks" = "tb"), ".tsv")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
              }

              if (!is.null(cv_result$predictions)) {
                write.table(cv_result$predictions, file = file.path(tmp_cv, paste0(gsub(" ", "_", sp), "_cv_preds_", switch(model_type, "suitable_habitat" = "sh", "native_range" = "nr"), "_", switch(method, "k-fold" = "k_fold", "spatial_blocks" = "sb", "temporal_blocks" = "tb"), ".tsv")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
              }

              if (!is.null(cv_result$fold_info) && method == "spatial_blocks") {
                fold_meta <- data.frame(
                  method = ifelse(is.null(cv_result$fold_info$method), NA, cv_result$fold_info$method),
                  block_method = ifelse(is.null(cv_result$fold_info$block_method), NA, cv_result$fold_info$block_method),
                  block_size_meters = ifelse(is.null(cv_result$fold_info$block_size), NA, cv_result$fold_info$block_size)
                )
                write.table(fold_meta, file = file.path(tmp_cv, paste0(gsub(" ", "_", sp), "_cv_info_", switch(model_type, "suitable_habitat" = "sh", "native_range" = "nr"), "_", switch(method, "k-fold" = "k_fold", "spatial_blocks" = "sb", "temporal_blocks" = "tb"), ".tsv")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
              }
            }
          }
        } else {
          warning(paste("Unable to download cross-validation results for ", sp, " as they have not been computed."))
        }
      }, error = function(e) {
        warning("Cross-validation export failed for ", sp, ": ", e$message)
      })
    }

    # Export presence/absence probability cutoffs if requested
    if (prob_cut){
      tryCatch({
        if (!all(sapply(pa_cutoff, is.null))){
          tmp_cutoff <- file.path(tmp_sp, paste(gsub(" ", "_", sp), "_pa_cutoff.tsv", sep = ""))
          sp_values <- lapply(pa_cutoff, function(x) x[[sp]])
          sp_values <- sp_values[!sapply(sp_values,is.null)]
          df <- data.frame(model = names(sp_values), prob_cutoff = unlist(sp_values), row.names = NULL)
          write.table(df, tmp_cutoff, quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
        } else {
          warning(paste("Unable to download P/A probability cutoff for ", sp, "as they have not been computed."))
        }
      }, error = function(e) {
        warning("Probability cutoff export failed for ", sp, ": ", e$message)
      })
    }

    # Add the temporary directory to the list of exported files
    export_files <- c(export_files, tmp_sp)
  }

  # Export config file if available
  if (!is.null(config_snapshot)) {
    tryCatch({
      config_path <- file.path(tempdir(), "glossa_config.json")
      jsonlite::write_json(config_snapshot, path = config_path, pretty = TRUE, auto_unbox = TRUE)
      export_files <- c(export_files, config_path)
    }, error = function(e) {
      warning("Configuration file export failed: ", e$message)
    })
  }

  if (is.null(export_files)) {
    tmp_empty <- file.path(tempdir(), "glossa_empty")
    dir.create(tmp_empty)
    export_files <- tmp_empty
  }

  return(export_files)
}

#' Create a Sparkline Value Box
#'
#' This function creates a custom value box with a sparkline plot embedded in it.
#'
#' @param title The title or heading of the value box.
#' @param sparkline_data The data used to generate the sparkline plot.
#' @param description A short description or additional information displayed below the value box.
#' @param type The type of sparkline plot to generate. Default is "line".
#' @param box_color The background color of the value box.
#' @param width The width of the value box. Default is 4.
#' @param elevation The elevation of the value box. Default is 0.
#' @param ... Additional parameters to be passed to the sparkline function.
#'
#' @return Returns a custom value box with the specified parameters.
#'
#' @keywords internal
sparkvalueBox <- function(title, sparkline_data, description, type = "line", box_color = "white", width = 4, elevation = 0, ...) {

  # Generate the sparkline plot
  if (type == "line"){
    # Calculate percentage increase and format description accordingly
    value <- sparkline_data[length(sparkline_data)]
    value <- ifelse(nchar(value) > 6, format(value, scientific = TRUE, digits = 2), value)
    first_period <- head(sparkline_data, ceiling(0.05*length(sparkline_data)))
    last_period <- tail(sparkline_data, ceiling(0.05*length(sparkline_data)))
    perc_inc <- round(((mean(last_period) - mean(first_period)) / mean(first_period)) * 100, 1)
    perc_inc <- ifelse(is.nan(perc_inc), 0, perc_inc)
    description <- paste0(ifelse(perc_inc >= 0, "+", ""), perc_inc, description)
    # Determine icon and colors based on percentage increase
    icon_name <- ifelse(perc_inc >= 0, "arrow-trend-up", "arrow-trend-down")
    icon_color <- ifelse(perc_inc >= 0, "green", "red")
    lineColor <- ifelse(perc_inc >= 0, "#4e8eed", "#E38CC0")
    fillColor <- ifelse(perc_inc >= 0, "#bcd3f5", "#F7D5EB")
    # Generate sparkline plot
    sparkline_plot <- sparkline(sparkline_data, type = "line", width = "100%", height = "50px", lineColor = lineColor, fillColor = fillColor, ...)
  } else if (type == "bar") {
    # Calculate ratio and format description accordingly
    value <- paste0(sparkline_data[1], "/", sparkline_data[2])
    ratio <- round(sparkline_data[1]/sparkline_data[2], 1)
    ratio <- ifelse(is.nan(ratio), 1, ratio)
    description <- paste0(ratio, description)
    # Determine icon color based on ratio
    icon_name <- "scale-balanced"
    if (ratio > 1){
      icon_name <- "scale-unbalanced"
    }
    if (ratio < 1) {
      icon_name <- "scale-unbalanced-flip"
    }
    icon_color <-  "#007bff"
    # No sparkline plot for bar type
    #sparkline_plot <- sparkline(sparkline_data, type = "bar", barWidth = "100%", height = "50px", chartRangeMin = 0, ...)
    sparkline_plot <- NULL
  }


  # CSS styling for the value box
  valueBox_css <- "
    .spark-value-and-sparkline {
      display: flex; /* or use display: grid; */
      align-items: center; /* if using flexbox */
    }

    .spark-box-number,
    .spark-box-sparkline {
      width: 50%; /* Adjust as needed */
      flex-grow: 1; /* Ensure equal width distribution */
    }

    .spark-box-number {
      font-size: 2em; /* Relative font size */
      font-weight: bold; /* Make the value bold */
    }

    /* Adjust the alignment or size of the sparkline_plot as needed */
    .spark-box-sparkline {
      /* Your styles for sparkline_plot */
    }

    .spark-box-description {
      text-align: right; /* Right-align the description */
      padding-right: 1em; /* Adjust as needed */
    }
  "

  # Create the custom value box div
  valueBoxCl <- paste0("small-box bg-", box_color)
  valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation) # Add elevation class
  custom_valueBox <- div(
    class = valueBoxCl,
    div(
      class = "inner",
      div(
        class = "info-box-text",
        title
      ),
      div(
        class = "spark-value-and-sparkline", # Parent div for value and sparkline_plot
        div(
          class = "spark-box-number",
          value
        ),
        div(
          class = "spark-box-sparkline",
          sparkline_plot
        )
      )
    ),
    div(
      class = "spark-box-description",
      icon(icon_name, style = paste0("color:", icon_color, ";")), # Add an icon (you can choose a different one)
      description
    )
  )

  # Return the value box div
  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    tags$style(HTML(valueBox_css)), # Include CSS styling
    custom_valueBox
  )
}

#' Create a Download Action Button
#'
#' This function generates a download action button that triggers the download of a file when clicked.
#'
#' @param outputId The output ID for the button.
#' @param label The label text displayed on the button. Default is "Download".
#' @param icon The icon to be displayed on the button. Default is NULL.
#' @param width The width of the button. Default is NULL.
#' @param status The status of the button. Default is NULL.
#' @param outline Logical indicating whether to use outline style for the button. Default is FALSE.
#' @param ... Additional parameters to be passed to the actionButton function.
#'
#' @return Returns a download action button with the specified parameters.
#'
#' @keywords internal
downloadActionButton <- function(outputId, label = "Download", icon = NULL,
                                 width = NULL, status = NULL, outline = FALSE, ...){
  # Generate the action button using actionButton from the bs4Dash package
  bttn <- bs4Dash::actionButton(
    inputId = paste0(outputId, "_bttn"),
    label = tagList(tags$a(id = outputId,
                           class = "btn shiny-download-link", href = "", target = "_blank",
                           download = NA), label),
    icon = icon,
    width = width,
    status = status,
    outline = outline,
    ...
  )

  # Append onclick attribute to the button to trigger download when clicked
  htmltools::tagAppendAttributes(bttn, onclick = sprintf("getElementById('%s').click()",
                                                         outputId))
}

#=========================================================#
# Plots ----
#=========================================================#

#' Generate Prediction Plot
#'
#' This function generates a plot based on prediction raster layers and presence/absence points.
#'
#' @param prediction_layer Raster prediction layer.
#' @param pa_points Presence/absence points.
#' @param legend_label Label for the legend.
#' @param non_study_area_mask Spatial polygon representing the non study areas.
#'
#' @return Returns a ggplot object representing the world prediction plot.
#'
#' @keywords internal
generate_prediction_plot <- function(prediction_layer, pa_points, legend_label, non_study_area_mask, coords) {
  p <- ggplot2::ggplot()

  # Add prediction layer if available
  if (!is.null(prediction_layer)) {
    if (legend_label == "potential_presences"){
      p <- p +
        tidyterra::geom_spatraster(data = terra::as.factor(prediction_layer)) +
        ggplot2::scale_fill_manual(values  = c("#65c4d8", "#f67d33"), name = legend_label)
    } else {
      lim <- switch(
        legend_label,
        "mean" = c(0, 1),
        "median" = c(0, 1),
        "sd" = NULL,
        "q0.025" = c(0, 1),
        "q0.975" = c(0, 1),
        "diff" = c(0, 1)
      )
      p <- p +
        tidyterra::geom_spatraster(data = prediction_layer) +
        ggplot2::scale_fill_gradientn(colours = c("#A1D4B1","#2BAF90","#F1A512","#DD4111","#8C0027"), na.value = "white",
                                      limits = lim, name = legend_label)
    }
  }

  # Add presence/absence points if available
  if (!is.null(pa_points)) {
    p <- p +
      ggplot2::geom_point(data = pa_points, aes(x = pa_points[, coords[1]], y = pa_points[, coords[2]], color = as.factor(pa_points[, "pa"])), alpha = 1) +
      ggplot2::scale_color_manual(values = c("0" = "black","1" = "green"), labels = c("Absences", "Presences"), name = NULL)
  }

  # Add non study area mask
  if (!is.null(non_study_area_mask)){
    p <- p +
      geom_sf(data = non_study_area_mask, color = "#353839", fill = "antiquewhite")
  }
  p <- p +
    theme(
      panel.grid.major = element_line(
        color = gray(.5),
        linetype = "dashed",
        linewidth = 0.5
      ),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

  return(p)
}

#' Plot cross-validation metrics
#'
#' This function generates a cross-validation radial plot based on evaluation metrics.
#'
#' @param data Dataframe containing cross-validation results.
#'
#' @return Returns a ggplot object representing the cross-validation plot.
#'
#' @keywords internal
plot_cv_metrics <- function(data){
  data <- data[, c("AUC", "CBI", "TSS", "ACC", "Fscore", "PREC", "SEN", "SPC", "FPR", "FDR")]
  data$CBI <- abs(data$CBI)
  data_mean <- colMeans(data, na.rm = TRUE)
  data_median <- apply(data, 2, function(x) median(x, na.rm = TRUE))
  data <- data.frame(id = seq_len(ncol(data)), metric = colnames(data), mean_value = data_mean, median_value = data_median)
  data$metric <- reorder(data$metric, data$mean_value)

  ggplot2::ggplot(data) +
    ggplot2::geom_col(
      aes(x = .data$metric, y = .data$mean_value, fill = .data$mean_value),
      position = "dodge2",
      show.legend = TRUE,
      alpha = 0.9
    ) +
    scale_fill_gradientn(
      colours = c("#aaf0e2", "#8fdbd4", "#73c5c6", "#58afb8", "#3c99aa", "#21839c", "#056d8e", "#005780", "#004172"),
      limits = c(0, 1),
      name = "mean"
    ) +
    ggplot2::geom_point(
      aes(x = .data$metric, y = .data$median_value, color = "median"),
      size = 3
    ) +
    scale_color_manual(values = "gray12", name = "") +
    ggplot2::geom_segment(
      aes(x = .data$metric, y = 0,
          xend = .data$metric, yend = 1),
      linetype = "dashed",
      color = "gray12"
    ) +
    coord_polar(start = 0) +
    annotate(
      x = 11,
      y = 0.25,
      label = "0.25",
      geom = "text",
      color = "gray12"
    ) +
    annotate(
      x = 11,
      y = 0.5,
      label = "0.5",
      geom = "text",
      color = "gray12"
    ) +
    annotate(
      x = 11,
      y =0.75,
      label = "0.75",
      geom = "text",
      color = "gray12"
    ) +
    annotate(
      x = 11,
      y =1,
      label = "1",
      geom = "text",
      color = "gray12"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0.1),
      breaks = c(0.25, 0.5, 0.75, 1)
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "gray12", size = 12)
    ) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
}

#' Plot cross-validation fold assignments
#'
#' @param data Dataframe with columns: 'decimalLongitude', 'decimalLatitude', 'pa' and 'fold'.
#' @param polygon An sf object representing the inverted study area.
#'
#' @return A ggplot object showing point color-coded by cv fold and shaped by presence/absence.
#' @export
plot_cv_folds_points <- function(data, polygon = NULL) {
  p <- ggplot2::ggplot()

  if (!is.null(polygon)){
    p <- p +
      geom_sf(data = polygon, color = "#353839", fill = "antiquewhite")
  }

  p <- p +
    ggplot2::geom_point(data = data, ggplot2::aes(x = .data$decimalLongitude, y = .data$decimalLatitude, color = as.factor(.data$fold), shape = as.factor(.data$pa)), size = 2, alpha = 0.8) +
    ggplot2::scale_color_viridis_d(name = "CV fold") +
    ggplot2::scale_shape_manual(name = "Observation", values = c("0" = 1, "1" = 4), labels = c("Absence", "Presence")) +
    ggplot2::coord_sf(datum = sf::st_crs(4326)) +
    theme(
      panel.grid.major = element_line(
        color = gray(.5),
        linetype = "dashed",
        linewidth = 0.5
      ),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
  )

  return(p)
}

# nocov end
