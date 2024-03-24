
#' Set the labels of a `field3logit` or a `multifield3logit` object
#'
#' It sets the labels of an existing `field3logit` or a `multifield3logit`
#' object.
#'
#' @param object a `field3logit` or a `multifield3logit` object.
#' @param value a character with the new label (or labels in case of a
#'   `multifield3logit` object).
#'
#' @returns Object of same class of argument `object` (either `field3logit` or
#' `multifield3logit`).
#'
#' @name labels
#'
#' @export
`labels<-` <- function(object, value) {
  UseMethod('labels<-')
}




