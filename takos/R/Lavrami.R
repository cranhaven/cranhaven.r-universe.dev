#'Title  Avrami Linearization
#' @description performs analysis of the thermograms using the linearized avrami method in the interval of Xc selected by the user
#' @param mat matrix of the all the thermograms checked using the functiom mat.check
#' @param up max degree of the interval for applying the linearized model default 0.9999
#' @param low min degree of the interval for applying the linearized model default 0.0001
#' @return models "mod", datable "xy" for plot
#' @references 1. Avrami M. Kinetics of Phase Change. I General Theory. J Chem Phys. 1939;7(12):1103-1112. doi:10.1063/1.1750380.
#' @export

lavrami <- function(mat, up=0.9999, low=0.0001) {
  time.minutes.check <- time.minutes<-time.minutes.zero <- id_cycle <- rate <- NULL

  mat.zero <- mat[mat$ri > 0][, time.minutes.check := (time.minutes.zero - min(time.minutes.zero)), by = id_cycle][]
  mat.zero <- mat.zero[ri < up & ri > low]
  Xc <- mat.zero$ri
  t.m <- mat.zero$time.minutes.check # a

  x <- log10(t.m)
  y <- log10(-log(1 - Xc))

  xy <- data.table(x, y, mat.zero$rate)
  setnames(xy, c("x", "y", "rate"))
  xy <- xy[is.finite(rowSums(xy)), ]
  formulas <- list(y ~ x)
  mod <- xy[, lapply(formulas, function(x) list(lm(x, data = .SD, ))), by = rate]
  my.list <- list("mod" = mod, "xy" = xy)

  print(summaryTableA(my.list))
  return(my.list)

}
