#' vostokR: Solar Potential Calculation for Point Clouds using VOSTOK
#'
#' This package provides an R interface to the VOSTOK (Voxel Octree Solar Toolkit) 
#' algorithm for calculating solar potential on LiDAR point clouds. It uses the 
#' original C++ implementation by Bechtold and Höfle (2020) with efficient ray 
#' casting and solar position algorithms to compute solar irradiance for each point, 
#' taking into account shadowing effects from surrounding points.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{calculate_solar_potential}} - Main function for solar potential calculation
#'   \item \code{\link{add_normals}} - Add normal vectors to point cloud  
#'   \item \code{\link{solar_ground_raster}} - Convert ground points to raster
#'   \item \code{\link{plot_solar_potential}} - Visualize results on point cloud
#'   \item \code{\link{set_vostokr_threads}} - Configure OpenMP thread count
#'   \item \code{\link{get_vostokr_threads}} - Get current thread configuration
#'   \item \code{\link{get_vostokr_performance_info}} - Check OpenMP capabilities
#'   \item \code{\link{clear_vostokr_caches}} - Clear performance caches
#' }
#' 
#' @section Citation:
#' When using this package, please cite both the R package and the original VOSTOK toolkit:
#' \itemize{
#'   \item Sánchez Meador, A.J. (2025). vostokR: Solar 
#'         Potential Calculation for Point Clouds using VOSTOK. R package version 0.1.1.
#'   \item Bechtold, S. and Höfle, B. (2020). VOSTOK - The Voxel Octree Solar Toolkit. 
#'         heiDATA, V1. https://doi.org/10.11588/data/QNA02B
#' }
#'
#' @references 
#' Bechtold, S. and Höfle, B. (2020). VOSTOK - The Voxel Octree Solar Toolkit. 
#' heiDATA. \doi{10.11588/data/QNA02B}
#'
#' @keywords internal
#' @name vostokR-package
#' @aliases vostokR-package
#' @useDynLib vostokR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods is
#' @importFrom stats setNames
"_PACKAGE"
