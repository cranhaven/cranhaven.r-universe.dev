

#' Heuristic.model.search.algorithm
#'
#' @include model_search_algorithm.R
#'
setClass("Heuristic.model.search.algorithm",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Model.search.algorithm"
)
