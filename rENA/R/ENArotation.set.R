#' ENARotationSet R6class
#
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export
#
#' @field rotation TBD
#' @field node.positions TBD
#' @field codes TBD
#' @field eigenvalues TBD
ENARotationSet = R6::R6Class("ENARotationSet",
  public = list(

    ## Public Functions ----
      #' Create ENARotationSet
      #'
      #' @param rotation TBD
      #' @param codes TBD
      #' @param node.positions TBD
      #' @param eigenvalues TBD
      #'
      #' @return ENARotationsSet
      initialize = function(
        rotation,
        codes,
        node.positions,
        eigenvalues = NULL
      ) {
        self$node.positions = node.positions;
        self$rotation = rotation;
        self$codes = codes;
        if(!is.null(codes) && !is.null(self$node.positions)) {
         rownames(self$node.positions) = codes;
        }
        self$eigenvalues = eigenvalues;
      },

    ## Public Properties ----
      rotation = NULL,
      node.positions = NULL,
      codes = NULL,
      eigenvalues = NULL
  ),
  private = list(
    #####
    ## Private Properties
    #####
      args = NULL
    #####
    ## END: Private Properties
    #####
  )
)
