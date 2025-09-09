#' unidimKLplot: KL distributions presented in the same units (Log10(LR))
#'
#' @param res output from distKL function.
#' @return A scatterplot.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom hrbrthemes theme_ipsum
unidimKLplot <- function(res) {
res2 <- mutate(res, KLpopped = - res$KLpopped)
res2 <- gather(res2)
res2
value <- key <- NULL
ggplot(data=res2, aes(x= value, group= key, fill= key)) +
  geom_density(adjust=1.5, alpha=.4) +
  hrbrthemes::theme_ipsum()}