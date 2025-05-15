#' @title Plot Inbag distribution
#' @description Plot distribution of the fraction of trees that samples were
#'   inbag in the Random Forest model.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object..
#' @param bins number of bins in histogram.
#' @param replace was sampling done with or without replacement?
#' @param sampsize sizes of samples drawn. Either a single value or vector of 
#'   sample sizes as long as the number of classes.
#' @param plot display the plot?
#'   
#' @note Red vertical lines on the plot denote the expected inbag rate(s). 
#'   These rates are based on the values of \code{replace} and 
#'   \code{sampsize} supplied. If not specified, they are set to the 
#'   \code{\link[randomForest]{randomForest}} defaults. If this is not the 
#'   same as the arguments used to run the model, there will be a mismatch in 
#'   the location of these indicator lines and the inbag frequency distribution.   
#'   
#' @return the \code{ggplot2} object is invisibly returned. 
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' sampsize = c(5, 5)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10)
#' plotInbag(rf)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 1000)
#' plotInbag(rf)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10000)
#' plotInbag(rf)
#' 
#' @export
#'
plotInbag <- function(x, bins = 10, replace = TRUE, sampsize = NULL, 
                      plot = TRUE) {
  rf <- as.randomForest(x)
  
  if(!is.null(sampsize)) {
    length.good <- length(sampsize) == 1 | 
      (rf$type == "classification" & length(sampsize) == length(unique(rf$y)))
    if(!length.good) {
      stop("sampsize' must be of length one or the number of classes in 'x'")
    }
  }
  
  n <- length(rf$y)
  if(is.null(sampsize)) sampsize <- if(replace) n else ceiling(0.632 * n)
  
  .pctPicked <- function(m) {
    pct <- 1 - (1 - (1 / m)) ^ m
    sapply(pct, function(p) if(is.nan(p)) 1 - (1 / exp(1)) else p)
  }
  
  exp.pct <- if(replace) {
    if(length(sampsize) == 1) {
      .pctPicked(sampsize) * 100
    } else {
      (as.vector((.pctPicked(sampsize) * sampsize)) / table(rf$y)) * 100
    }
  } else if(length(sampsize) == 1) {
    (sampsize / n) * 100
  } else {
    as.vector(sampsize / table(rf$y)) * 100
  }
  
  pct <- ((rf$ntree - rf$oob.times) / rf$ntree) * 100
  bins <- max(bins, floor(length(pct) / 5))
  p <- data.frame(pct = pct) |> 
    ggplot2::ggplot(ggplot2::aes_string("pct")) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(
      x = "Percent of trees where sample was inbag",
      y = "Frequency"
    ) + 
    ggplot2::geom_vline(xintercept = exp.pct, color = "red")
  
  if(plot) print(p)
  invisible(p)
}