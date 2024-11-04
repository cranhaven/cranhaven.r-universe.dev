#' Selecting variables using linear regression
#'
#' @description Function for selecting variables using linear regression
#'
#' @usage selectvarlm(y, x, ctr.vif = 10)
#'
#' @param y A vector of a response variable
#' @param x A data.frame of explanatory variables
#' @param ctr.vif A number of VIF threshold
#'
#' @return A data.frame of selected variables
#'
#' @importFrom stats cor
#' @importFrom methods is
#'
#' @examples
#' data("obs")
#' data("sample_vars_sda")
#' obs$Cr_ppm <- log(obs$Cr_ppm)
#' krm <- rmvoutlier(obs$Cr_ppm)
#' y <- obs$Cr_ppm[-krm]
#' x <- sample_vars_sda$Elevation[-krm, 1:11]
#' sx <- selectvarlm(y, x)
#'
#' @export
#'

selectvarlm <- function(y, x, ctr.vif = 10){

  ## rank x by correlations ### debug
  c1 <- cor(cbind(y, x), method = "pearson")
  c2 <- abs(c1[1, -1])
  order.x <- rev(order(c2))
  x3 <- x[,order.x]

  xs <- x3
  ys <- y

  nys <- length(ys)
  nxs <- ncol(xs)

  m <- c()
  for (i in 2:nxs){
    k <- 1:i
    if (length(m) > 0) {
      k <- k[-m]
    }

    x5 <- xs[, k]
    f1.vif <- vif(x5)

    if (max(f1.vif) > ctr.vif){ ## remove multicollinearity
      m <- c(m, i)
    }
  }

  if (!is.null(m)){
    xk <- c(1:nxs)[-m]
  } else {
    xk <- c(1:nxs)
  }

  selected.x.data <- x3[, xk]

  if (is(selected.x.data, "data.frame")){
    # debug: convert a vector to dataframe
    # debug: use is()
    selected.x.data <- data.frame(selected.x.data)
    names(selected.x.data) <- names(x)[xk]
  }

  return(selected.x.data)
}


