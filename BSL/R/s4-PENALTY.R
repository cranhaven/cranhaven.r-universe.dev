#' @include s4-MODEL.R
NULL

#' S4 class ``PENALTY''
#' @description This S4 class contains the penalty selection result from
#'   function \code{\link{selectPenalty}}. \code{show} display the penalty
#'   selection result. \code{plot} plot the penalty selection result using
#'   ggplot2.
#' @rawRd \Rdversion{1.1}
#' @slot loglike A list of the log-likelihood values. The list contains multiple
#'   matrices (each corresponds to the result for a specific n value). The
#'   number of row of the matrix equals to the number of repeats \code{M}. The
#'   columns of the matrix stands for different penalty values.
#' @slot n A vector of \code{n}, the number of simulations from the model per
#'   MCMC iteration for estimating the synthetic likelihood.
#' @slot lambda A list, with each entry containing the vector of penalty values
#'   for the corresponding choice of \code{n}.
#' @slot M The number of repeats used in estimating the standard deviation of
#'   the estimated log synthetic likelihood.
#' @slot sigma The standard deviation of the log synthetic likelihood estimator
#'   to aim for, usually a value between 1 and 2. This reflects the mixing of a
#'   Markov chain.
#' @slot model A ``MODEL'' object generated with function \code{newModel}.
#'   See \code{\link{newModel}}.
#' @slot stdLoglike A list contains the estimated standard deviations of
#'   log-likelihoods.
#' @slot penalty The vector stores the selected penalty values for each given
#' \code{n} by choosing from the candidate \code{lambda} list. The selected
#' values produce closest standard deviations \code{stdLoglike} to the target
#' \code{sigma}.
#' @slot result The result data frame.
#' @slot call The original code used to run \code{\link{selectPenalty}}.
#' @seealso \code{\link{selectPenalty}} for the function that selects the
#'   penalty parameter.
#' @aliases PENALTYclass
#' @export
PENALTY <- setClass("PENALTY",
                    slots = c(loglike = "list",
                              n = "numeric",
                              lambda = "list",
                              M = "integer",
                              sigma = "numeric",
                              model = "MODEL",
                              stdLoglike = "list",
							  penalty = "numeric",
                              result = "data.frame",
                              call = "call")
)

setMethod("initialize",
          signature = "PENALTY",
          definition = function(.Object, loglike, n, lambda, sigma, model, call) {
            .Object@loglike <- loglike
            .Object@M <- nrow(loglike[[1]])
            if (missing(sigma)) {
              .Object@sigma <- 1.5
              cat("Set the target sigma value as 1.5 by default\n")
            } else {
              .Object@sigma <- sigma
            }
            .Object@n <- n
            .Object@lambda <- lambda
            if (!missing(model)) .Object@model <- model
            if (!missing(call)) .Object@call <- call
            validObject(.Object)
            .Object <- computePenaltyResult(.Object)
            return(.Object)
          }
)

setValidity("PENALTY",
            method = function(object) {
              if (!is.list(object@loglike)) {
                return("No loglike list found")
              }
              if (length(object@n) != length(object@lambda)) {
                return("Length of n mismatch the length of lambda")
              }
              if (length(object@loglike) != length(object@n)) {
                return("Length of the loglike list mismatch the length of n")
              }
              if (any(sapply(object@loglike, FUN = nrow) != object@M)) {
                return("M mismatch the loglike list")
              }
              if (any(sapply(object@loglike, FUN = ncol) != sapply(object@lambda, FUN = length))) {
                return("lambda mismatch the loglike list")
              }
              if (object@sigma <= 0) {
                return("The target sigma value must be positive, a value between 0 and 1 is recommended")
              }
              TRUE
            }
)

# #' @description Find the closest penalty value to the target sigma and format
# #' the result into a data frame. Notice that the closest value is not
# #' necessarily a local minimum or maximum.
setGeneric("computePenaltyResult", function(object) standardGeneric("computePenaltyResult"))

setMethod("computePenaltyResult",
          signature = c("PENALTY"),
          definition = function(object) {
		    N <- length(object@n)
		    stdLoglike <- vector("list", N)
            idxClosest <- integer(N)
            for (i in 1 : N) {
              temp <- apply(object@loglike[[i]], FUN = sd, MARGIN = 2)
              stdLoglike[[i]] <- temp
              idxClosest[i] <- which.min(abs(temp - object@sigma))[1]
            }
            object@stdLoglike <- stdLoglike
			object@penalty <- sapply(object@lambda, min)

            result <- vector("list", N)
            for (i in 1 : N) {
              isClosest <- logical(length(object@stdLoglike[[i]]))
              isClosest[idxClosest[i]] <- TRUE
              result[[i]] <- data.frame(n = object@n[i], penalty = object@lambda[[i]], logPenalty = log(object@lambda[[i]]),
                                        stdLoglike = object@stdLoglike[[i]], isClosest = isClosest)
            }
            result <- Reduce(rbind, result)
            object@result <- result
            return(object)
          }
)

#' @param object The S4 object of class ``PENALTY'' to show.
#' @rdname PENALTY-class
#' @export
setMethod("show",
          signature = c(object = "PENALTY"),
          definition = function(object) {
            digits = max(3L, getOption("digits") - 4L)
            cat("\nCall:\n", paste(deparse(object@call), sep = "\n", collapse = "\n"),
                "\n\n", sep = "")
            if (length(object@result)) {
              r1 <- object@result[object@result$isClosest, c("n", "penalty", "stdLoglike")]
              cat("Penalty selected based on the standard deviation of the loglikelihood:\n")
              print(format(r1, digits = digits))
            } else {
              cat("No result to show\n")
            }
            return(invisible(r1))
          }
)

#' @param x The S4 object of class ``PENALTY'' to plot.
#' @param logscale A logical argument whether the x-axis (penalty) should be log transformed. The
#' default is \code{TRUE}.
#' @rdname PENALTY-class
setMethod("plot",
          signature = c(x = "PENALTY"),
          definition = function(x, logscale = TRUE) {
            penalty <- logPenalty <- stdLoglike <- isClosest <- NULL
            result <- x@result
            n <- x@n
            a <- floor(sqrt(length(n)))
            b <- ceiling(length(n) / a)
            nRepeats <- sapply(x@lambda, length)
            yPosSigma <- sapply(n, FUN = function(xx) mean(range(result[result$n == xx, "stdLoglike"])))
            textYSigma <- c(unlist(mapply(yPosSigma, nRepeats, FUN = rep)))
            result$isClosest[!result$isClosest] <- NA

            if (logscale) {
              ggplot(data = result, aes(x = logPenalty, y = stdLoglike)) +
                geom_line(color = "darkblue", linetype = "dashed", size = 1) +
                facet_wrap( ~ n, scales = "free", nrow = a, ncol = b, labeller = label_both) +
                geom_vline(aes(xintercept = logPenalty * isClosest), na.rm = TRUE, color = "forestgreen", linetype = 4) +
                geom_label(aes(x = logPenalty * isClosest, y = textYSigma, label = paste("penalty == ", round(penalty, 3))),
                           hjust = 0.5, vjust = "inward", parse = TRUE, color = "white", fill = "#FE66A9", size = 2.7,
                           alpha = 0.8, na.rm = TRUE) +
                labs(x = "log penalty", y = "standard deviation of log-likelihood", title = "Penalty Selection") +
                theme(plot.title = element_text(size = 14, hjust = 0.5)) +
                theme(strip.text.x = element_text(size = 12, face = "bold"), axis.title = element_text(size = 12))
            } else {
              ggplot(data = result, aes(x = penalty, y = stdLoglike)) +
                geom_line(color = "darkblue", linetype = "dashed", size = 1) +
                facet_wrap( ~ n, scales = "free", nrow = a, ncol = b, labeller = label_both) +
                geom_vline(aes(xintercept = penalty * isClosest), na.rm = TRUE, color = "forestgreen", linetype = 4) +
                geom_label(aes(x = penalty * isClosest, y = textYSigma, label = paste("penalty == ", round(penalty, 3))),
                           hjust = 0.5, vjust = "inward", parse = TRUE, color = "white", fill = "#FE66A9", size = 2.7,
                           alpha = 0.8, na.rm = TRUE) +
                labs(x = "penalty", y = "standard deviation of log-likelihood", title = "Penalty Selection") +
                theme(plot.title = element_text(size = 14, hjust = 0.5)) +
                theme(strip.text.x = element_text(size = 12, face = "bold"), axis.title = element_text(size = 12))
            }
          }
)


#' Obtain the selected penalty values from a "PENALTY" object
#' @description see \code{\link{PENALTYclass}}
#' @param object   A ``PENALTY'' class object.
#' @param ... Other arguments.
#' @return The selecty penalty values.
setGeneric("getPenalty", function(object, ...) standardGeneric("getPenalty"))
#' @rdname PENALTY-class
#' @export
setMethod("getPenalty",
          signature = c(object = "BSL"),
          definition = function(object) {
              object@penalty
          }
)
