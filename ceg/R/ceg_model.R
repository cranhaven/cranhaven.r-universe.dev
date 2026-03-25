


#' Ceg.model S4 class
#'
#' \code{Ceg.model} is a S4 class whose objects represent a Chain-Event Graph
#' (CEG) model, which is composed by a Staged Tree object and its corresponding staged structure.

#'
#' @slot staged.tree Staged.tree S4 object
#' @slot position list
#'
#'
#'@include staged_tree.R
setClass("Ceg.model",
         representation(staged.tree = "Staged.tree",
                        position = "list")
)



setMethod(
  f = "initialize",
  signature = "Ceg.model",
  definition = function(.Object,
                        staged.tree,
                        postion) {
    cat("~~~ CegModel: initializator ~~~ \n")
    # Assignment of the slots
    .Object@staged.tree <- staged.tree
    .Object@postion <- postion
    return(.Object)
    # return of the object
  }
)



