


#' Exhaustive.model.search.algorithm S4 Class
#'
#' @include model_search_algorithm.R
#'
# @export
#'
setClass("Exhaustive.model.search.algorithm",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Model.search.algorithm"
)
