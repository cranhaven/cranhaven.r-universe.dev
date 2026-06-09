#' @name tree_inventory_chablais3
#' 
#' @title Tree inventory data in France (Chablais 3 plot, July 2010)
#' 
#' @description All trees with diameter at breast height >= 7.5 cm are inventoried on a 50m x 50m plot.
#' 
#' @docType data
#'
#' @usage data(tree_inventory_chablais3)
#'
#' @format A \code{data.frame} with columns:
#' \enumerate{
#' \item \code{x} easting coordinate (epsg: 2154)
#' \item \code{y} northing coordinate (epsg: 2154)
#' \item \code{d} dbh (cm)
#' \item \code{h} tree height (m)
#' \item \code{n} tree number
#' \item \code{s} species abreviated as GESP (GEnus SPecies)
#' \item \code{e} appearance (0: missing or lying, 1: normal, 2: broken treetop, 3: dead with branches, 4: snag)
#' \item \code{t} tilted (0: no, 1: yes)
#' }
#'
#' @keywords datasets
#'
#' @references Monnet, J.-M. 2011. Using airborne laser scanning for mountain forests mapping: Support vector regression for stand parameters estimation and unsupervised training for treetop detection. Ph.D. thesis. University of Grenoble, France. pp. 21-22 & 34 \url{https://theses.hal.science/tel-00652698/document}
#'
#' @examples
#' data(tree_inventory_chablais3)
#' summary(tree_inventory_chablais3)
#' # display tree inventory
#' plot_tree_inventory(tree_inventory_chablais3[, c("x", "y")],
#'   diam = tree_inventory_chablais3$d, col = "red",
#'   pch = tree_inventory_chablais3$e,
#'   xlab = "X", ylab = "Y"
#' )
NULL
"tree_inventory_chablais3"
