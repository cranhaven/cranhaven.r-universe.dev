Class.sample <- function(lbls, nsam=NULL, prop=NULL, uniform=FALSE) {
 if (is.null(nsam) & is.null(prop)) stop("either 'nsam' or 'prop' must be specified")
 if (!uniform) {
  if (!is.null(prop)) nsam <- prop * ave(seq_along(lbls), lbls, FUN=function(.x) length(.x))
  ave(seq_along(lbls), lbls, FUN=function(.x) sample.int(length(.x))) <= nsam
 } else {
  if (!is.null(prop)) {
   ave(seq_along(lbls), lbls,
    FUN=function(.x) seq_along(.x) %in% round(seq(1, length(.x), length.out=round(length(.x) * prop)))) > 0
  } else {
   ave(seq_along(lbls), lbls,
     FUN=function(.x) seq_along(.x) %in% round(seq(1, length(.x), length.out=floor(nsam)))) > 0
  }
 }
}
