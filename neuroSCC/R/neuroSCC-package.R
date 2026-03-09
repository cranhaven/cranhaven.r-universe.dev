#' Wrappers for Neuroimaging Functional Data Preparation and SCC Analysis
#'
#' @description
#' The \code{neuroSCC} package provides tools to preprocess and structure neuroimaging data
#' for functional data analysis using Simultaneous Confidence Corridors (SCCs). It wraps external packages
#' to prepare data from PET images, extract contours, generate meshes, and evaluate regions of statistical significance.
#'
#' The methods implemented support both group comparisons and single-subject vs. group inference,
#' following the methodology described in Wang et al. (2020) and the author's PhD thesis.
#'
#' @details
#' This package serves as a bridge between neuroimaging file formats (e.g., NIfTI) and advanced
#' statistical tools like \code{ImageSCC::scc.image}. It includes the following key components.
#' \itemize{
#'   \item Loading and cleaning PET image data.
#'   \item Extracting ROIs and constructing functional data matrices.
#'   \item Generating synthetic Poisson clones for 1-vs-group settings.
#'   \item Extracting SCC-detected points and evaluating detection metrics.
#' }
#'
#' @docType package
#' @name neuroSCC-package
#' @aliases neuroSCC
#' @seealso \code{\link{neuroCleaner}}, \code{\link{databaseCreator}}, \code{\link{getPoints}}
"_PACKAGE"
