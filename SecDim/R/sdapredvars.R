#' Preparing explanatory variables data for SDA-based prediction
#'
#' @description Function for preparing explanatory variables data for
#' SDA-based prediction or the testing data for cross validation
#'
#' @usage sdapredvars(xlist)
#'
#' @param xlist A list of the SDA explanatory variables
#'
#' @return A data.frame of variables for prediction or validation
#'
#' @examples
#' data("obs")
#' data("sample_vars_sda")
#' obs$Cr_ppm <- log(obs$Cr_ppm)
#' krm <- rmvoutlier(obs$Cr_ppm)
#' y <- obs$Cr_ppm[-krm]
#' x <- list(sample_vars_sda[[1]][-krm, 1:11])
#' kvalidate <- sample(length(y), 0.3*length(y), replace = FALSE)
#' yv <- y[kvalidate]
#' xv <- lapply(x, function(x) x[kvalidate,])
#' sdaxv <- sdapredvars(xv)
#' sdayxv <- cbind(yv, sdaxv)
#'
#' @export
#'
sdapredvars <- function(xlist){
  nx <- length(xlist)
  selected.vars <- list()
  for (i in 1:nx){
    dx <- xlist[[i]]
    dx <- dx[, c(1:ncol(dx))]

    names(dx) <- paste(paste("v", i, sep = ""), names(dx), sep = "")

    selected.vars[[i]] <- dx
  }
  vars <- do.call(cbind, selected.vars)
  return(vars)
}
