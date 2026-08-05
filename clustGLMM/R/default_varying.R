default_varying <- function(...) {
  varying <- c(prec_num = TRUE, c_ord = TRUE,
               InvSigma = FALSE, InvQ = FALSE,
               naY = FALSE)
  varying <- set_dots(varying, ..., type = "logical")
  return(varying)
}
