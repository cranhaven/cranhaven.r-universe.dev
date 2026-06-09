
#' Add Reflectance column to LAS if it is missing for RIEGL vz400i
#'
#' Function to provide relative Reflectance for RIEGL vz400i. Lidar prediction
#' of crown scorch is based on relative range-corrected
#' reflectance relative to a white reference object orthonormal to
#' scanner. Raw range-corrected amplitudes from RIEGL vz400i are linearly
#' correlated to relative intensity which usually ranges from -20 dB to 0 dB
#' @param las `LAS` object from `lidR` package representing an individually
#' segmented tree containing an `Intensity` column representing 16-bit range-
#' corrected amplitude from RIEGL vz400i Terrestrial Lidar Scanner
#' @examples
#' library(lidR)
#' library(CrownScorchTLS)
#'
#'  #download external data from github repo
#' url <- paste0(
#'   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/",
#'   "M-04-15549_post.laz")
#'  las_file = tempfile(fileext = paste0(".", tools::file_ext(url)))
#'  download.file(url, las_file, mode = "wb", quiet = TRUE)
#'  las <- readLAS(las_file)
#'
#'  # or load your own data
#'  #las <- readLAS('C:/path/to/your/file.laz')
#'
#' las = add_reflectance(las)
#' colnames(las@data)
#' @return modified LAS object with Reflectance column
#' @importFrom lidR add_lasattribute
#' @export
add_reflectance = function(las) {
  cols = colnames(las@data)
  if('Reflectance' %in% cols) return(las)
  ref = -25 + 4.577804e-4*las$Intensity
  las = lidR::add_lasattribute(las, ref, 'Reflectance', 'Reflectance')
  return(las)
}

#' Generate histogram of Reflectance for prediction with random forests
#'
#' Generates a histogram of Reflectance intensities for prediction with
#' Random Forests. Histogram breaks can be defined.
#' @param las `LAS` object from `lidR` package representing an individually
#' segmented tree containing a `Reflectance` column representing relative
#' reflectance from from RIEGL vz400i Terrestrial Lidar Scanner. See `add_reflectance()`
#' @param breaks sequence of breaks for histograms, default from Cannon et al. 2025.
#' @examples
#' library(lidR)
#' library(CrownScorchTLS)

#'  #download external data from github repo
#' url <- paste0(
#'   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/",
#'   "M-04-15549_post.laz")
#'  las_file = tempfile(fileext = paste0(".", tools::file_ext(url)))
#'  download.file(url, las_file, mode = "wb", quiet = TRUE)
#'  las <- readLAS(las_file)
#'
#'  # or load your own data
#'  #las <- readLAS('C:/path/to/your/file.laz')
#'
#' las = add_reflectance(las)
#' histogram = get_histogram(las)
#' plot(density ~ intensity, data=histogram, xlab='Reflectance (dB)', type='l')
#' @importFrom graphics hist
#' @return data.frame with columns intensity and density
#' @export
get_histogram = function(las, breaks = seq(-20,0, by = 0.2)) {
  if(is.null(las$Reflectance)) stop('las does not contain Reflectance column. Use add_reflectance() function to calculate it from Intensity')
  ref = las$Reflectance
  ref = ref[ref < max(breaks)]
  ref = ref[ref > min(breaks)]
  histogram = hist(ref, breaks=breaks, plot = FALSE)
  histogram = data.frame(intensity = histogram$mids,
                         density = histogram$density)
  return(histogram)
}

#' Remove tree bole from `LAS`
#'
#' This function identifies and removes tree boles using the
#' `TreeLS` package available at \url{https://github.com/tiagodc/TreeLS}
#' @param las `LAS` object from `lidR` package representing an individually
#' segmented tree
#' @examples
#' library(lidR)
#' library(CrownScorchTLS)
#'
#' #'  #download external data from github repo
#' url <- paste0(
#'   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/",
#'   "M-04-15549_post.laz")
#'  las_file = tempfile(fileext = paste0(".", tools::file_ext(url)))
#'  download.file(url, las_file, mode = "wb", quiet = TRUE)
#'  las <- readLAS(las_file)
#'
#'  # or load your own data
#'  #las <- readLAS('C:/path/to/your/file.laz')
#'
#' #plot(las)
#' crown_only = remove_stem(las)
#' #plot(crown_only)
#' @return LAS object with stem removed
#' @importFrom lidR filter_poi
#' @importFrom stats quantile
#' @export
remove_stem = function(las) {
    las$Z = las$Z - quantile(las$Z,0.001)
    las = stemPoints(las)
    las = lidR::filter_poi(las, !las$Stem)
    las = lidR::filter_poi(las, las$Z > 1)
    return(las)
}

#' Predict canopy scorch from `LAS` tree object following Cannon et al. 2025
#'
#' This function follows methods in Cannon et al. 2025 to predict crown scorch
#' of a `LAS` object representing an individual tree collected using a RIEGL vz400i
#' Terrestrial Lidar system. The function uses the 'relative reflectance' (in decibels)
#' and predicts crown scorch using `randomForests` following Cannon et al. 2025
#' @param las `LAS` object from `lidR` package representing an individually
#' segmented tree collected from RIEGL vz400i Terrestrial Lidar Scanner
#' @param model `randomForests` model object containing histogram data generated
#' from `get_histogram` function. if `model` is `NULL`, then default model
#' from Cannon et al. 2025 is used. But custom model may be generated.
#' @param plot Boolean indicating whether reflectance histogram should be plotted
#' @examples
#' library(lidR)
#' library(CrownScorchTLS)
#'
#' #download external data from github repo
#' url <- paste0(
#'   "https://raw.githubusercontent.com/jbcannon/CrownScorchTLS-data/main/data/manual-clip-trees/",
#'   "M-04-15549_post.laz")
#' las_file = tempfile(fileext = paste0(".", tools::file_ext(url)))
#' download.file(url, las_file, mode = "wb", quiet = TRUE)
#' las <- readLAS(las_file)
#'
#' # or load your own data
#' #las <- readLAS('C:/path/to/your/file.laz')
#'
#' predict_scorch(las) #using default model from Cannon et al. 2025
#' @return predicted scorch as numeric vector
#' @import randomForest
#' @importFrom tidyr pivot_wider
#' @importFrom stats predict
#' @importFrom utils installed.packages
#' @export
predict_scorch = function(las, model = NULL, plot=FALSE) {

  #require randomForests package to be installed
  pkgs = installed.packages()
  if(!'randomForest' %in% pkgs[,1]) {
    stop("`randomForest` packages is not installed. Use `install.packages('randomForest')` to install it.")
  }
  # require randomForests package to be attached
  if('randomForest' %in% .packages()) {

  }


  if(is.null(model)) {
    message('Loading default random forest prediction model\n')
    model_file = system.file('extdata', 'RF_scorch_int.RDS', package = 'CrownScorchTLS')
    model = readRDS(model_file)
  }

  message('removing stem using stemPoints()\n')
  las = remove_stem(las)

  # Ensure Reflectance column exists, add if not
  if(!'Reflectance' %in% colnames(las@data)) {
    message('Reflectance column does not exist. Adding using the add_reflectance() function')
    las = add_reflectance(las)
  }

  # Generate reflectance histogram
  histogram = get_histogram(las)
  histogram$intensity = round(histogram$intensity, 1)
  if(plot) plot(density ~ intensity, data = histogram, type='l', xlab='Reflectance (dB)')

  # Make scorch prediction
  prediction_data = tidyr::pivot_wider(histogram, names_from = 'intensity', values_from = 'density', names_prefix='intensity_')
  predicted_scorch = predict(model, newdata=prediction_data)
  names(predicted_scorch) = 'predicted_scorch'
  return(predicted_scorch)
}

