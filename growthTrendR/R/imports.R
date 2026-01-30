# utils.R
# ----------------------------
# Centralized package imports for growthTrendR
# ----------------------------


#' @importFrom future plan multicore multisession sequential
#' @importFrom furrr future_map
#' @importFrom pryr mem_used object_size

#' @importFrom utils combn write.csv head capture.output


#' @import data.table
#' @importFrom dplyr group_by mutate ungroup row_number summarise n
#' @import stringr
#' @import stats
#' @import mgcv
#' @import nlme
#'

#' @importFrom terra rast crs mask vect xyFromCell values
#' @importFrom terra crop writeRaster ncell project

#' @importFrom raster extent crs crs<- raster xyFromCell
#' @importFrom raster crop clamp plot crs crs<-
#' @importFrom grDevices png dev.off colorRampPalette gray.colors dev.cur
#' @importFrom graphics par rect axis layout mtext grid
#' @importFrom curl curl



#' @import patchwork
#' @import ggplot2
#' @import htmltools


# #' @import gstat


NULL
