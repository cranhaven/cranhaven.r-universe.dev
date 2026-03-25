
#' Dirchlet.MPNL.distribution
#'
#' @include dirchlet_distribution.R
#'
#' @slot score numeric.
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot cluster list.
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#'
#'
setClass("Dirchlet.MPNL.distribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Dirchlet.distribution"
)
