#' Prepare string for regular expressions 
#' (backslash for all non-letter and non-digit characters)
#' 
#' @export
#' @param text A text string (smooth term label) that needs to be converted 
#' to a regular expression. 
#' @return A regular expression string.
#' @author Jacolien van Rij
#' @examples
#' data(simdat)
#' # Model for illustrating coefficients:
#' m0 <- bam(Y ~ s(Time) + s(Subject, bs='re') 
#' + s(Time, Subject, bs='re'), data=simdat)
#' 
#' # get all coefficients:
#' coef(m0)
#' # to get only the Subject intercepts:
#' coef(m0)[grepl(convertNonAlphanumeric('s(Subject)'), names(coef(m0)))]
#' # to get only the Subject slopes:
#' coef(m0)[grepl(convertNonAlphanumeric('s(Time,Subject)'), names(coef(m0)))]
#'
#' @family Utility functions
convertNonAlphanumeric <- function(text) {
    return(gsub("([^a-zA-Z0-9])", "\\\\\\1", as.character(text)))
}





#' Calculate the correlation between the fitted model and data.
#' 
#' @export
#' @import mgcv
#' @param model A fitted regression model (using gam, or bam).
#' @return Numeric value: correlation between fitted model and data. 
#' @examples 
#' data(simdat)
#'
#' # Fit simple GAM model:
#' gam1 <- bam(Y ~ s(Time), data=simdat, discrete=TRUE)
#' corfit(gam1)
#' 
#' @family Utility functions
corfit = function(model) {
    if ("gamm" %in% class(model)) {
        stop(sprintf("Apply the function on the gam part of the model: modeledf(%s$gam)", deparse(substitute(model))))
    } else if ((!"gam" %in% class(model))) {
        stop("Model is not a gam object. Currently this function only works for models build with bam(), gam(), or gamm().")
    }
    return(cor(model$model[, 1], fitted(model)))
}





#' Compare the formulas of two models and return the difference(s). 
#' 
#' @export
#' @import mgcv
#' @param model1 A fitted regression model (using lm, glm, gam, or bam).
#' @param model2 A fitted regression model (using lm, glm, gam, or bam).
#' @return A list with model terms that are not shared by both models.
#' @author Jacolien van Rij
#' @examples 
#' data(simdat)
#'
#' # Fit simple GAM model:
#' gam1 <- bam(Y ~ s(Time), data=simdat)
#' gam2 <- bam(Y ~ Group+s(Time), data=simdat)
#' diff_terms(gam1, gam2)
#'
#' @family Utility functions
diff_terms <- function(model1, model2) {
    fa <- c()
    fb <- c()
    get_formula <- function(x) {
        if ("gam" %in% class(x)) {
            return(attr(terms(x$formula), "term.labels"))
        } else {
            stop(sprintf("This function does not work for model of class %s.", class(x)))
        }
    }
    fa <- get_formula(model1)
    fb <- get_formula(model2)
    d1 <- fa[!fa %in% fb]
    d2 <- fb[!fb %in% fa]
    out <- list()
    out[[deparse(substitute(model1))]] <- d1
    out[[deparse(substitute(model2))]] <- d2
    return(out)
}





#' Retrieve the degrees of freedom specified in the model.
#' 
#' @export
#' @import mgcv
#' @param model A fitted regression model (using gam, or bam).
#' @return Numeric value: degrees of freedom specified in the model.
#' @examples 
#' data(simdat)
#' 
#' \dontrun{
#' # models take somewhat longer time to run:
#' 
#' # Fit simple GAM model:
#' gam1 <- bam(Y ~ s(Time), data=simdat, discrete=TRUE)
#' modeledf(gam1)
#' gam2 <- bam(Y ~ s(Time)+s(Time, Subject, bs='fs', m=1), 
#'     data=simdat, discrete=TRUE)
#' modeledf(gam2)
#' gam3 <- bam(Y ~ Subject+s(Time, by=Subject), 
#'     data=simdat, discrete=TRUE)
#' modeledf(gam3)
#' gam4 <- bam(Y ~ Group+s(Time)+s(Time, Subject, bs='fs', m=1), 
#'     data=simdat, discrete=TRUE)
#' modeledf(gam4) 
#' gam5 <- bam(Y ~ Group+s(Time, by=Group)+s(Time, Subject, bs='fs', m=1), 
#'     data=simdat, discrete=TRUE)
#' modeledf(gam5) 
#' 
#' # Fit a gamm:
#' gam6 <- gamm(Y ~ Group+s(Time), random=list(Subject=~1) data=simdat, discrete=TRUE)
#' # this produces an error...
#' modeledf(gam6)
#' # ... but this works:
#' modeledf(gam6$gam)
#' }
#' 
#' @family Utility functions
modeledf = function(model) {
    if ("gamm" %in% class(model)) {
        stop(sprintf("Apply the function on the gam part of the model: modeledf(%s$gam)", deparse(substitute(model))))
    } else if ((!"gam" %in% class(model))) {
        stop("Model is not a gam object. Currently this function only works for models build with bam(), gam(), or gamm().")
    }
    null.space.dim <- 0
    if (length(model$smooth) > 0) {
        null.space.dim <- sum(sapply(model$smooth, FUN = function(x) {
            x$null.space.dim
        }, USE.NAMES = FALSE))
    }
    return(length(model$sp) + model$nsdf + null.space.dim)
}
#' Number of observations in the model.
#' 
#' @export
#' @import mgcv
#' @param model A fitted regression model (using gam, bam, (g)lm, (g)lmer).
#' @return Numeric value: number of observations that are considered by the 
#' model. 
#' @examples 
#' data(simdat)
#' # simulate some missing data:
#' simdat[sample(1:nrow(simdat), size=15),]$Y <- NA
#' simdat[sample(1:nrow(simdat), size=7),]$Group <- NA
#' 
#' # Fit simple GAM models:
#' gam1 <- bam(Y ~ s(Time), data=simdat, discrete=TRUE)
#' gam2 <- bam(Y ~ Group + s(Time, by=Group), data=simdat, discrete=TRUE)
#' 
#' # number of data points in data frame:
#' nrow(simdat)
#' 
#' # observations model gam1:
#' observations(gam1)
#' # observations model gam2:
#' observations(gam2)
#' 
#' @family Utility functions
observations = function(model) {
    if ("gamm" %in% class(model)) {
        stop(sprintf("Apply the function on the gam part of the model: observations(%s$gam)", deparse(substitute(model))))
    } else if ("lme" %in% class(model)) {
        return(nrow(model@frame))
    } else if ("lm" %in% class(model)) {
        return(nrow(model$model))
    } else if (("gam" %in% class(model))) {
        return(nrow(model$model))
    } else {
        stop("Currently this function only works for models build with bam(), gam(), glm(), lm(), lmer(), and glmer().")
    }
}
#' Retrieve the residual degrees of freedom from the model.
#' 
#' @export
#' @import mgcv
#' @param model A fitted regression model (using gam, or bam).
#' @return Numeric value: residual degrees of freedom from the model.
#' @examples 
#' data(simdat)
#'
#' # Fit simple GAM model:
#' gam1 <- bam(Y ~ s(Time), data=simdat, discrete=TRUE)
#' res_df(gam1)
#' # ... which is the same as:
#' 
#' modeledf(gam1)
#' 
#' @family Utility functions
res_df = function(model) {
    if ("gamm" %in% class(model)) {
        stop(sprintf("Apply the function on the gam part of the model: res_df(%s$gam)", deparse(substitute(model))))
    } else if ((!"gam" %in% class(model))) {
        stop("Model is not a gam object. Currently this function only works for models build with bam(), gam(), or gamm().")
    }
    return(nrow(model$model) - modeledf(model))
}





#' Return the regions in which the smooth is significantly different from zero.
#' 
#' @export
#' @import grDevices
#' @import graphics
#' @param mean A vector with smooth predictions.
#' @param se A vector with the standard error on the smooth predictions.
#' @param xVals Optional vector with x values for the smooth. 
#' When \code{xVals} is provided, the regions are returned in terms of x-
#' values, otherwise as indices.
#' @param f A number to multiply the \code{se} with, to convert the \code{se} 
#' into confidence intervals. Use 1.96 for 95\% CI and 2.58 for 99\%CI.
#' @param as.vector Logical: whether or not to return the data points as 
#' vector, or not. Default is FALSE, and a list with start and end points will
#'  be returned.
#' @return The function returns a list with start points of each region 
#' (\code{start}) and end points of each region (\code{end}). The logical 
#' \code{xVals} indicates whether the returned values are on the x-scale 
#' (TRUE) or indices (FALSE).
#' @examples
#' data(simdat)
#' 
#' # Use aggregate to calculate mean and standard deviation per timestamp:
#' avg <- aggregate(simdat$Y, by=list(Time=simdat$Time),
#'     function(x){c(mean=mean(x), sd=sd(x))})
#' head(avg)
#' # Note that column x has two values in each row:
#' head(avg$x)
#' head(avg$x[,1])
#' 
#' # Plot line and standard deviation:
#' emptyPlot(range(avg$Time), c(-20,20), h0=0)
#' plot_error(avg$Time, avg$x[,'mean'], avg$x[,'sd'], 
#'    shade=TRUE, lty=3, lwd=3)
#'
#' # Show difference with 0:
#' x <- find_difference(avg$x[,'mean'], avg$x[,'sd'], xVals=avg$Time)
#' # Add arrows:
#' abline(v=c(x$start, x$end), lty=3, col='red')
#' arrows(x0=x$start, x1=x$end, y0=-5, y1=-5, code=3, length=.1, col='red')
#' @author Jacolien van Rij
#'
#' @family Utility functions
find_difference <- function(mean, se, xVals = NULL, f = 1, as.vector = FALSE) {
    if (length(mean) != length(se)) {
        stop("The vectors mean and se are not equal in length.")
    } else {
        ub <- mean + f * se
        lb <- mean - f * se
        
        n <- which(!(ub >= 0 & lb <= 0))
        if (as.vector) {
            if (length(n) == 0) {
                return(rep(FALSE, length(mean)))
            } else {
                out <- rep(FALSE, length(mean))
                out[n] <- TRUE
                return(out)
            }
        } else {
            if (length(n) == 0) {
                return(NULL)
            } else {
                n_prev <- c(NA, n[1:(length(n) - 1)])
                n_next <- c(n[2:length(n)], NA)
                if (!is.null(xVals) & (length(xVals) == length(mean))) {
                  return(list(start = xVals[n[which(is.na(n - n_prev) | (n - n_prev) > 1)]], end = xVals[n[which(is.na(n_next - 
                    n) | (n_next - n) > 1)]], xVals = TRUE))
                } else {
                  return(list(start = n[which(is.na(n - n_prev) | (n - n_prev) > 1)], end = n[which(is.na(n_next - 
                    n) | (n_next - n) > 1)], xVals = FALSE))
                }
                
            }
        }
    }
}





#' Return indices of data that were not fitted by the model.
#' 
#' @export
#' @param model A fitted regression model (using lm, glm, gam, or bam).
#' @return The indices of the data that were not fitted by the model.
#' @author Jacolien van Rij
#' @examples 
#' data(simdat)
#'
#' # Add missing values:
#' set.seed(123)
#' simdat[sample(nrow(simdat), size=20),]$Y <- NA
#' # Fit simple linear model:
#' lm1 <- lm(Y ~ Time, data=simdat)
#' na.el <- missing_est(lm1)
#' length(na.el)
#'
#' @family Utility functions
missing_est <- function(model) {
    if ("lm" %in% class(model)) {
        el <- unique(model$na.action)
        if (length(el) > 0) {
            return(sort(el))
        } else {
            return(el)
        }
    } else {
        stop("This method is currently only implemented for lm, glm, gam, and bam models.")
    }
}





#' Return a list with reference levels for each factor.
#' 
#' @export
#' @description Function for retrieving all reference levels.
#' @param data Data
#' @return Named list with reference levels for each factor.
#' @author Jacolien van Rij
#' @family Utility functions
refLevels <- function(data) {
    var.factor <- names(data)[sapply(data, function(x){ inherits(x, 'factor')})]
    if(length(var.factor) > 0){
    	return( lapply(data[ ,var.factor], function(x){
    		return(list(ref=levels(x)[1], contr=contrasts(x)))
    	}) )
    }else{
    	return(list())
    }
}





#' Print a descriptive summary of a data frame.
#' 
#' @export
#' @description The function prints a summary of the data. 
#' Similar to the function \code{\link[utils]{str}}, but easier readable.
#' @param data A data frame.
#' @param print Logical: whether or not to print the summary.
#' @param n Number: maximum number of values being mentioned in the summary.
#' If NULL all values are being mentioned. Defaults to 10.
#' @return Optionally returns a named list with info.
#' @author Jacolien van Rij
#' @examples
#' data(simdat)
#' summary_data(simdat)
#' @family Utility functions
summary_data <- function(data, print = TRUE, n = 10) {
    
    labelColumns <- function(x, data) {
        out <- NULL
        cn <- ifelse(is.numeric(x), colnames(data)[x], x)
        cl <- class(data[, cn])
        if (inherits(data[, cn], "factor")) {
            vals <- sort(unique(as.character(data[, x])))
            n.cur <- length(vals) + 1
            if (!is.null(n)) {
                n.cur <- n
            }
            if (length(vals) > n.cur) {
                out <- sprintf("factor with %d values; set to the value(s): %s, ...", length(vals), paste(vals[1:n.cur], 
                  collapse = ", "))
            } else {
                out <- sprintf("factor; set to the value(s): %s.", paste(vals, collapse = ", "))
            }
        } else if (inherits(data[, cn], "numeric")) {
            if (length(unique(data[, x])) > 2) {
                out <- sprintf("numeric predictor; with %d values ranging from %f to %f.", length(unique(data[, 
                  x])), min(data[, x], na.rm = TRUE), max(data[, x], na.rm = TRUE))
            } else {
                out <- sprintf("numeric predictor; set to the value(s): %s.", paste(unique(data[, x]), collapse = ", "))
            }
        } else if (inherits(data[, cn], "matrix")) {
            if (length(unique(data[, x])) > 2) {
                out <- sprintf("a matrix predictor; with %d values ranging from %f to %f.", length(unique(data[, 
                  x])), min(data[, x], na.rm = TRUE), max(data[, x], na.rm = TRUE))
            } else {
                out <- sprintf("matrix predictor; set to the value(s): %s.", paste(unique(data[, x]), collapse = ", "))
            }
        } else {
            vals <- sort(unique(data[, x]))
            n.cur <- length(vals) + 1
            if (!is.null(n)) {
                n.cur <- n
            }
            if (length(vals) > n.cur) {
                out <- sprintf("%s vector with %d values; set to the value(s): %s, ...", class(data[, cn])[1], 
                  length(vals), paste(vals[1:n.cur], collapse = ", "))
            } else {
                out <- sprintf("%s vector; set to the value(s): %s.", class(data[, cn])[1], paste(vals, collapse = ", "))
            }
        }
        return(out)
    }
    mysummary <- sapply(colnames(data), function(x) {
        labelColumns(x, data)
    })
    if (print) {
        print_summary(mysummary)
    }
    invisible(mysummary)
}
#' Print a named list of strings, output from \code{\link{summary_data}}.
#' 
#' @export
#' @param sumlist Named list, output of \code{\link{summary_data}}.
#' @param title Optional, text string that will be printed as title.
#' @author Jacolien van Rij
#' @family Utility functions
print_summary <- function(sumlist, title = NULL) {
    if (is.null(title)) {
        cat("Summary:\n")
    } else {
        cat(title, "\n")
    }
    for (x in names(sumlist)) {
        cat("\t*", x, ":", sumlist[[x]], "\n")
    }
}





#' Label timestamps as timebins of a given binsize.
#' 
#' @export
#' @description Function for calculating timebins.
#' @param x Numerical vector with timestamp information.
#' @param binsize Size of the timebin, measured in the same units (often ms) 
#' as \code{x}.
#' @param pos Numerical value that determines the label of the binsize 
#' as proportion of the binsize. A value of 0 will provide the minimum 
#' timestamp within the bin as label, a value of 1 will provide the maximum 
#' value within the bin as label. Defaults to 0.5, the center of the bin.
#' @return Anumerical vector of the same size as \code{x} with timebin 
#' information.
#' @author Jacolien van Rij
#' @examples
#' data(simdat)
#' # grouping Time values in bins:
#' simdat$Timebin <- timeBins(simdat$Time, 200)
#' head(simdat)
#' 
#' # different labels:
#' simdat$Timebin2 <- timeBins(simdat$Time, 200, pos=0)
#' head(simdat)
#' @family Utility functions
timeBins <- function(x, binsize, pos = 0.5) {
    return((floor(x/binsize) + pos) * binsize)
}





