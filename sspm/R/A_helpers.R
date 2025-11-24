# Exported ----------------------------------------------------------------

#' Get the list of available discretization methods
#'
#' Currently, only one discretization method is supported:
#'     * `"tesselate_voronoi"` Voronoi tessellation using the function
#'       [tesselate_voronoi][tesselate_voronoi].
#'
#' You can create your own method (tutorial TBD).
#'
#' @return
#' A `character vector` of all available discretization methods.
#'
#' @export
spm_methods <- function() {
  choices <- c('tesselate_voronoi', 'triangulate_delaunay')
  return(choices)
}

#' Get the list of available smoothing methods
#'
#' Currently, only one smoothing method is supported:
#'     * `"ICAR"`: Intrinsic Conditional Auto-Regressive models.
#'     * `"LINPRED"`: LINear PREDictors (lag smooths).
#'
#' @return
#' A `character vector` of all available smoothing methods.
#'
#' @export
spm_smooth_methods <- function() {
  choices <- c('ICAR', 'LINPRED')
  return(choices)
}

# Not exported ------------------------------------------------------------

# assert that a column is part of a data_frame
assert_column <- function(df,  col){

  checkmate::assert_data_frame(df)
  checkmate::assert_character(col)

  if (!checkmate::test_subset(col, names(df))) {
    stop(paste0("`", deparse(substitute(col)),
                "` must be a column of `", deparse(substitute(df)), "`"),
         call. = FALSE)
  }

}

# Retrieves the 3 possible aggregation choices
spm_aggregation_choices <- function() {
  choices <- c('space', 'time', 'spacetime')
  return(choices)
}

# Retrieves the 2 possible level of aggregation choices
spm_aggregation_levels_choices <- function(){
  choices <- c("patch", "boundary")
  return(choices)
}

# Retrieves the 2 possible level of aggregation types
spm_aggregation_types_choices <- function(){
  choices <- c("data", "smoothed")
  return(choices)
}

# Suppress both messages and warnings
suppressAll <- function(x) {
  suppressWarnings(suppressMessages(x))
}
