# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
#-------------------------------------------------------------------------------
#' Add vegetation indices on a IRC image
#'
#' Computes vegetation indices from the Red, Green and Infra-Red bands of an IRC
#' image and adds them as additional bands or columns. Acronyms are listed on
#' \url{https://www.nv5geospatialsoftware.com/docs/BroadbandGreenness.html}. If the 
#' Blue band is also present, additional indices are computed.
#'
#' @param r raster or data.frame. Should contain bands or columns with
#' names nir, r, g (and b)
#' @param all boolean. indicates whether all indices should be computed;
#' default:FALSE, only grvi, sr and ndvi are calculated
#' @param scale numeric. values in bands are scaled from range [0, scale] to [0, 1]
#' @return a raster or data.frame with added bands or columns
#' @examples
#' df <- data.frame(nir = c(110, 150, 20),
#' r = c(25, 50, 30),
#' g = c(10, 60, 10),
#' b = c(20, 60, 0))
#' add_vegetation_indices(df, all = TRUE)
#' @export
add_vegetation_indices <- function(r, all = FALSE, scale = 255) {
  # scale values from 0-scale to 0-1
  if (!is.null(scale)) 
  {
    r <- r / scale
  }
  # Green Ratio Vegetation Index (GRVI)
  r$grvi <- r$nir / r$g
  # Simple ratio (SR)
  r$sr <- r$nir / r$r
  # Normalized Difference Vegetation Index (NDVI)
  r$ndvi <- (r$nir - r$r) / (r$nir + r$r)
  #
  # additional indices
  if (all) {
    # Difference Vegetation Index (DVI)
    r$dvi <- r$nir - r$r
    # Green Difference Vegetation Index (GDVI)
    r$gdvi <- r$nir - r$g
    # Green Normalized Difference Vegetation Index (GNDVI)
    r$gndvi <- (r$nir - r$g) / (r$nir + r$g)
    # Green Optimized Soil Adjusted Vegetation Index (GOSAVI)
    r$gosavi <- (r$nir - r$g) / (r$nir + r$g + 0.16)
    # Green Soil Adjusted Vegetation Index (GSAVI)
    r$savi <- 1.5 * (r$nir - r$g) / (r$nir + r$g + 0.5)
    # Infrared Percentage Vegetation Index (IPVI)
    r$ipvi <- r$nir / (r$nir + r$r)
    # modified non-linear index (MNLI)
    r$mnli <- (r$nir^2 - r$r) * (1 + 0.5) / (r$nir^2 + r$r + 0.5)
    # Modified Soil Adjusted Vegetation Index 2 (MSAVI2)
    r$msavi2 <- (2 * r$nir + 1 - sqrt((2 * r$nir +1)^2 - 8 * (r$nir - r$r)))/2
    # Modified Simple Ratio (MSR)
    r$msr <- ((r$nir / r$r) - 1) / (sqrt(r$nir / r$r) - 1)
    # Non-Linear Index (NLI)
    r$nli <- (r$nir^2 - r$r) / (r$nir^2 + r$r)
    # Optimized Soil Adjusted Vegetation Index (OSAVI)
    r$osavi <- (r$nir - r$r) / (r$nir + r$r + 0.16)
    # Renormalized Difference Vegetation Index (RDVI)
    r$rdvi <- (r$nir - r$r) / sqrt(r$nir + r$r)
    # Soil Adjusted Vegetation Index (SAVI)
    r$savi <- (1.5 * (r$nir - r$r)) / (r$nir + r$r + 0.5)
    # Transformed Difference Vegetation Index (TDVI)
    r$tdvi <- sqrt(0.5 + (r$nir - r$r) / (r$nir + r$r))
    #
    # if blue band is present, other indices
    if (is.element("b", names(r)))
    {
      # Brightness
      r$brightness <- r$r + r$g + r$b
      r$brightness2 <- sqrt(r$r^2 + r$nir^2)
      # shadow index
      r$si <- ((1 - r$r) * (1 - r$g) * (1 - r$b))^(1/3)
      # Enhanced Vegetation Index (EVI)
      r$evi <- 2.5 * (r$nir - r$r) / (r$nir + 6 * r$r - 7.5 * r$b + +1)
      # Green Leaf Index (GLI)
      r$gli <- (2 * r$g - r$r - r$b) / (2 * r$g + r$r + r$b)
      # Leaf Area Index (LAI)
      LAI = (3.608 * r$evi - 0.118)
      # Green ratio
      r$gratio <- r$g / (r$nir + r$r + r$g + r$b)
      # NIR ratio
      r$nirratio <- r$nir / (r$nir + r$r + r$g + r$b)
    }
  }
  r
}
