#' Visualization of Longitudinal Data Analysis
#'
#' Visualization of multidimensional longitudinal data based on the projection method using the indicator matrix.
#'
#'
#' @param x A data frame consisting of categorical data coded in numbers. Its \code{n} samples(\code{object}) should have been repeatedly measured through multiple time points; its \code{p} variables will be represented as variable coordinate. To keep track of which observation occurred in which time point, you must have included a variable, \code{Time}.
#' @param object A vector of length n samples. The \code{object} who would have made repeatedly measure through multiple time points; the object is indicated by the name of the observation coordinate.
#' @param time A time point of longitudinal data. Accepts a character string that denotes the name of the time variable.
#' @param type A type of longitudinal data.
#'
#'
#' @return
#'    \item{obs.coordinate}{A tibble data class of row coordinates. Each row represents row coordinates and the observations corresponding to each row are included in the \code{obs_list}}
#'    \item{var.coordinate}{The column coordinate.}
#'    \item{Eigen}{Summarize the \code{principal inertias(Eigenvalues)} that as a result of applying the above algorithm using the indicator matrix}
#'    \item{GOF}{\code{Goodness-of-fit} of the Approximation for 2-dimensional VLDA plot.}
#'
#'
#' @details
#' The value returned by vlda is using as the main argument of vlda_plot and vlda_add function, the corresponding model.
#' long-format is that each row is one time point per \code{object} So each \code{object} has \code{T} rows. All \code{T} values for each \code{object} are stacked–they're all in the one column; wide-format is that a \code{object} repeated responses will be in a single row, and each response is in a separate column. so \eqn{(Y_{1}, \ldots ,Y_{T})} are the response variables obtained at time \eqn{t(=1,\ldots ,T)}.\code{type = c(long, wide)}
#'
#' @seealso \code{vlda_add} \cr \code{vlda_plot}
#' @keywords VLDA
#' @examples
#' ## longform of the PTSD data
#' data(PTSD)
#' PTSD <- as.data.frame(PTSD)
#' PTSD[,2:4] <- apply(PTSD[,2:4], 2, function(x) ifelse(x >= 3, 1, 0))
#' PTSD[,5] <-  ifelse(PTSD[,5] >= 6 , 1, 0)
#' PTSD <- data.frame(lapply(PTSD, function(x) as.factor(x)))
#' vlda(x = PTSD, object = "subject", time = "time", type = "long")
#'
#'
#' ## Wideform od the Depression data
#' data(Depression)
#' head(Depression)
#' vlda(Depression, object = "Case", time = c("1week", "2weeks", "4weeks"), type = "wide")
#' vlda(Depression, "Case", c("1week", "2weeks", "4weeks"), "wide")
#'
#' @import dplyr
#' @importFrom utils combn globalVariables
#' @export vlda


vlda <- function(x, object , time, type = c("long", "wide")){

  if( class(x) != "data.frame" ) stop( "Data should be data.frame" )

  if( is.null(object) ) stop( "argument 'object' is missing, with no default" )

  if( is.null(time) ) stop( "argument 'time' is missing, with no default" )

  if( is.null(type) ) stop( "argument 'type' is missing, with no default" )

  if ( type == "long" ){

    name <- paste( x[, object], x[, time], sep = "_" )

    id = paste( x[ , object] )

  } else if( type == "wide" ){

    name <- paste( x[ ,object] )

  }

  X <- x[ ,-which(colnames(x) == object) ]

  if(any( apply(X, 2, function(x) length(table(x)) > 10 ) ) ) warning(paste0( "\nPlease check the format of column ",

                                                                              which(apply(x, 2, function(x) length(table(x)) > 10 ) ) ) )

  if(any( apply(X, 2, function(x) length(table(x)) > 100 ) ) ) stop(paste0( "\nPlease check the format of column ",

                                                                            which(apply(x, 2, function(x) length(table(x)) > 100 ) ) ) )


  ind.mat <- indicator(X)

  Z <- as.matrix(ind.mat)

  S <- Z / sum(Z)

  rm <- apply(S, 1, sum)

  cm <- apply(S, 2, sum)

  Dr <- diag(1 / sqrt(rm))

  Dc <- diag(1 / sqrt(cm))

  G <- Dr %*% S %*% Dc

  svd.G <- svd(G)

  lam <- (svd.G$d[-1]) ^ 2

  expl <- 100 * (lam / sum(lam))

  Eigen <- rbind(round(lam, 3), round(expl, 1))

  rownames(Eigen) <- c( "Eigenvalue", "Percent" )

  Eigen <- data.frame(t(Eigen))

  Eigen <- subset(Eigen, Eigenvalue != 0)

  Eigen$Cumulative <- cumsum(Eigen[, 2])

  Eigen$Percent <- paste( Eigen$Percent, "%" )

  Eigen$Cumulative <- paste( Eigen$Cumulative, "%" )

  GOF <- paste( "Goodness of fit :", Eigen[2, 3] )


  Q <- svd.G$v

  var.coordinate <- Dc %*% Q

  var.coordinate <- round(var.coordinate[, 2:3], 3)

  rownames(var.coordinate) <- colnames(Z)

  colnames(var.coordinate) <- c("x", "y")

  P <- svd.G$u

  obs.raw <- obs.coordinate <- round((Dr %*% Dr %*% S %*% Dc %*% Q)[, 2:3], 3)

  obs.coordinate <- as.data.frame(obs.coordinate)

  colnames(obs.coordinate) <- c("x", "y")

  obs.coordinate$name <- name

  obs.coordinate <- summarise(group_by(obs.coordinate, x, y), obs_list = list(name))

  i <-  1
  for ( i in 1 : nrow(obs.coordinate)){

    names(obs.coordinate$obs_list)[i] <- paste0( "x = ",obs.coordinate[i, 1]," ", "y = ", obs.coordinate[i, 2] )

  }



  XLAB <- paste0( "Dim1(", round(expl[1], 2), "%)" )

  YLAB <- paste0( "Dim2(", round(expl[2], 2), "%)" )
  LAB <- c(XLAB, YLAB)

  output <- list(obs.coordinate = obs.coordinate, var.coordinate = var.coordinate, Eigen = Eigen, GOF = GOF,
                 x = x, object = object, time = time, type = type, name = name, svd.G = svd.G,
                 obs.raw = obs.raw, ind.mat = ind.mat, LAB = LAB)

  class(output) <- "vlda"

  attr(output, "hidden") <- c("svd.G", "x", "object", "time", "type", "name", "ind.mat", "obs.raw", "LAB")

  output
}






