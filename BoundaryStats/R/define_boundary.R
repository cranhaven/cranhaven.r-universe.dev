#' @name define_boundary
#' @title Define the boundary elements of a SpatRaster with numeric data or boundary intensities
#' @description
#'
#' Defines boundaries in a SpatRaster object by keeping a proportion of the cells with the highest
#' boundary intensity values. If the SpatRaster contains trait values, the values can be converted
#' to boundary/edge values (convert = T) using a Sobel-Feldman operator.
#'
#' @param x A SpatRaster object.
#' @param threshold A value between 0 and 1. The proportion of cells to keep as boundary elements. default = 0.2.
#' @param convert logical. If TRUE, convert values of each cell from trait values to boundary intensities. default = FALSE.
#'
#' @return A SpatRaster object with cell values 1 for boundary elements and 0 for other cells
#' 
#' @examples \donttest{
#' data(grassland)
#' grassland <- terra::rast(grassland_matrix, crs = grassland_crs)
#' terra::ext(grassland) <- grassland_ext
#' 
#' grassland_boundaries <- define_boundary(grassland, 0.1)
#' }
#' 
#' @author Amy Luo
#' @references
#' Fortin, M.J. et al. (2000) Issues related to the detection of boundaries. Landscape Ecology, 15, 453-466.
#' Jacquez, G.M., Maruca,I S. & Fortin M.-J. (2000) From fields to objects: A review of geographic boundary analysis. Journal of Geographical Systems, 3, 221, 241.
#' @export
define_boundary <- function (x, threshold = 0.2, convert = FALSE) {
  # if the raster contains trait values, estimate first partial derivatives of the cells in lon and lat directions
  if (convert == TRUE) {x <- sobel_operator(x)}

  # sort the cell values from highest to lowest, then find the value above which only the threshold
  # proportion of cells would be kept
  threshold_value <- terra::values(x) %>%
    na.omit(.) %>%
    sort(., decreasing = TRUE) %>%
    head(., round(length(.) * threshold)) %>%
    min(.)

  # Check proportion of cells above threshold value. Sometimes there are a lot of redundant cell values, so if there
  # are redundancies at the threshold value, then the proportion of cells kept will be higher than the threshold.
  prop <- terra::values(x, dataframe = TRUE) %>%
    na.omit(.) %>%
    .[. >= threshold_value] %>%
    length(.)/length(na.omit(terra::values(x)))

  # so we'll keep reducing the threshold value until the proportion of cells kept is below the threshold
  if (threshold_value %% 1 == 0) {                # sometimes the boundary values are integers
    while (prop > threshold) {
      threshold_value = threshold_value + 1       # so increase the threshold value by 1
      prop <- terra::values(x) %>%
        na.omit(.) %>%
        .[. >= threshold_value] %>%
        length(.)/length(na.omit(terra::values(x)))
      }
    } else {                                      # sometimes the boundary values are float
    rep = 0
    while (prop > threshold) {
      threshold_value = threshold_value * 1.1     # so increase the threshold value by 10%
      prop <- terra::values(x, dataframe = TRUE) %>%               # until the proportion of cells kept < threshold
        na.omit(.) %>%
        .[. >= threshold_value] %>%
        length(.)/length(na.omit(terra::values(x)))
      rep = rep + 1
      if (rep >= 10) {break}                      # break after 10 reps if it gets that far
      }
    }

  # make raster with boundary elements (filter out cells with values below threshold)
  boundaries <- terra::as.matrix(x, wide = T)
  for (row in 1:nrow(boundaries)) {
    for (col in 1:ncol(boundaries))
      if(!is.na(boundaries[row, col])) {
        if (boundaries[row, col] < threshold_value) {boundaries[row, col] = 0} else {boundaries[row, col] = 1}
      }
  }

  # remove singleton boundary elements
  for (row in 1:nrow(boundaries)) {
    for (col in 1:ncol(boundaries)) {
      if (!is.na(boundaries[row, col])) {
        if(boundaries[row, col] == 1) {
          # name all neighboring cells
          if (row > 1) {up = boundaries[row - 1, col]} else {up = NA}
          if (row < nrow(boundaries)) {down = boundaries[row + 1, col]} else {down = NA}
          if (col > 1) {left = boundaries[row, col - 1]} else {left = NA}
          if (col < ncol(boundaries)) {right = boundaries[row, col + 1]} else {right = NA}

          if (row > 1 & col > 1) {upleft = boundaries[row - 1, col - 1]} else {upleft = NA}
          if (row > 1 & col < ncol(boundaries)) {upright = boundaries[row - 1, col + 1]} else {upright = NA}
          if (row < nrow(boundaries) & col > 1) {downleft = boundaries[row + 1, col - 1]} else {downleft = NA}
          if (row < nrow(boundaries) & col < ncol(boundaries)) {downright = boundaries[row + 1, col + 1]} else {downright = NA}

          # test whether any neighbors are boundary elements
          neighbors <- c(up, down, left, right, upleft, upright, downleft, downright)

          # if not, then set the focal cell to 0 (change focal cell to not boundary element)
          if(1 %in% neighbors == F) {boundaries[row, col] = 0}
        }
      }
    }
  }

  boundaries <- terra::rast(boundaries)
  terra::ext(boundaries) <- terra::ext(x)
  return(boundaries)

}
