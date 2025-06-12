#' @name paramplot.OEFPIL
#' @title Plot parameters of an OEFPIL object
#' @description Function for plotting the estimated values of the parameters with error bars (plus minus standard deviation) using \code{ggplot} for an object (or list of objects) of class \code{"OEFPIL"}.
#'
#' @usage paramplot.OEFPIL(object)
#'
#' @param object an object or a \code{list} of objects of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#'
#' @details The input list has to be without \code{NaN}, \code{NA}, \code{Inf} or \code{-Inf} values in the estimated parameters or covariance matrix in the source \code{"OEFPIL"} object. In that case the function returns a warning message and no graph is plotted.
#'
#' @return A ggplot graph of the estimated parameter values with error bars. The result can be edit using other ggplot components as usually.
#'
#' @note Due to possible large differences in units of estimated parameters, the \code{scale} argument for facetting in the \code{ggplot} graph is set to \code{"free"}. It should be taken into account when interpreting the results.
#'
#' @seealso \code{\link{OEFPIL}}, \code{\link{curvplot.OEFPIL}} and \code{\link{plot.OEFPIL}}.
#'
#' @examples
#' \dontshow{
#' utils::example("coef.OEFPIL",echo=FALSE)}
#' ##-- Continuing the coef.OEFPIL(.) example:
#'
#' n <- nrow(steamdata)
#' CM2 <- diag(c(rep(0.2^2,n), rep(0.1^2,n)))
#' st2 <- OEFPIL(steamdata, y ~ b1 * 10^(b2 * x/ (b3 + x)), list(b1 = 5, b2 = 8, b3 = 200),
#'              CM2, useNLS = FALSE)
#'
#' ##Example 1 - Use of paramplot.OEFPIL function on an object of class 'OEFPIL'
#' paramplot.OEFPIL(st2)
#'
#' ##Example 2 - Use of paramplot.OEFPIL function on a list of objects of class 'OEFPIL'
#' paramplot.OEFPIL(list(st1,st2))
#'
#'
#' @import ggplot2
#' @export



paramplot.OEFPIL <- function(object){
  ## Function for graph of estimated parameters with error bars (plus minus standard deviation)
  ## with ggplot
  ## object . . . list of 'OEFPIL' objects

  if(!is.list(object)){
    stop("Input has to be a list.")
  } # control of an input

  out.list <- lapply(object, function(i){i[names(i) != "logs" & names(i) != "cov.m_nlsLM"]})

  if(!IsListOK(out.list)){
    stop("There are NA or NaN in estimated parameter values or in the covariance matrix.")
  } # control of NaN values

  # creating data structure for ggplot
  N <- length(object)

  if (is.vector(object[[1]])){
    # data structure for only one 'OEFPIL' object input
    o.form <- object[names(object) != "logs" & names(object) != "cov.m_nlsLM"]
    if(!IsListOK(o.form)){
      stop("There are NA or NaN in estimated parameter values or in the covariance matrix.")
    }
    coefnames <- object$contents$names.of.parameters
    k <- length(coefnames)
    data <- matrix(NA, nrow = k, ncol = 4)
    data <- as.data.frame(data)
    data[,1] <- rep(1, each = k)
    data[,2] <- coefnames
    data[,3] <- unlist(object[1:k])
    data[,4] <- sqrt(diag(object$cov.m_Est))
  } else {
    # data structure for list of 2 or more 'OEFPIL' objects
    coefnames <- object[[1]]$contents$names.of.parameters
    k <- length(coefnames)
    data <- matrix(NA, nrow = k*N, ncol = 4)
    data <- as.data.frame(data)
    data[,1] <- rep(1:N, each = k)
    data[,2] <- rep(coefnames,N)
    for (i in 1:N){
      l <- k*i -k +1
      data[l:(k*i),4] <- sqrt(diag(object[[i]]$cov.m_Est))
      data[l:(k*i),3] <- unlist(object[[i]][1:k])
    }
  }

  model <- cf <- est <- sdest <- NULL
  names(data[,3]) <- NULL
  colnames(data) <- c("model","cf", "est", "sdest")
  data$model <- as.factor(data$model)

  # plotting graph with ggplot
  ggplot(data, aes(x = model, y = est, col = model)) +
    geom_pointrange(aes(ymin = est - sdest, ymax =  est + sdest))+
    labs(x = "", y = "", title = "Estimation of parameters with standard deviation") +
    facet_wrap(~ cf, scales = "free") +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}
