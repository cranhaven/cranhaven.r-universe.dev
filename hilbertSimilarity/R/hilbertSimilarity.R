#' Hilbert Similarity Index for High Dimensional Data
#'
#' This package provides a method to compute similarity between single cell samples in
#' high dimensional space. After dividing the space into voxels, each sample is summarized as a
#' number of cells per voxel. Voxels are ordered using a Hilbert curve, so that each sample can be
#' represented as a 1-dimensional density plot. the distance between 2 samples corresponds to
#' the Jensen Shannon distance between the 2 probability vectors.
#'
#' @example examples/example.js.dist.R
#'
#' @docType package
#' @name hilbertSimilarity
NULL
