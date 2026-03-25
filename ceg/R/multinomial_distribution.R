
#' Multinomial.distribution
#'
#' @include  distribution_of_probability.R
#'
#' @slot score numeric.
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot cluster list.
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#'
setClass("Multinomial.distribution",
     #    representation(score = "numeric", cluster = "list"),
         contains = "Distribution.of.probability"
)
