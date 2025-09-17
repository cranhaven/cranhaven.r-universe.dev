#' Pediatric T-ALL Clinical Data from COG trial AALL0434
#'
#' The beam.data object used in example beam analyses
#'
#' @format ## `beam_dat`
#' A beam.data object, which is a list with the following elements:
#' \describe{
#'   \item{main.data}{A data.frame with clinical/endpoint data.}
#'   \item{mtx.data}{A list of the omics data matrices.}
#'   \item{mtx.anns}{A list of omic annotation data.frames.}
#'   \item{anns.mtch}{A data.frame with information to link mtx.data and mtx.anns.}
#'   \item{set.data}{A data.frame with set.id, mtx.id, and row.id to link omic features to sets.}
#'   \item{set.anns}{Optional data.frame with set annotation data.}
#'   \item{boot.index}{A matrix with bootstrap indices.}
#' }
#' @source NA
"beam_dat"
