#' Print function for class rfdata
#'
#' This methods displays the basic information about the noise
#' filtering process contained in an object of class \code{rfdata}.
#'
#' This function presents the basic information of the regression noise filter and the resulting
#' noisy dataset contained in the object \code{x} of class \code{rfdata}.
#' The information offered is as follows:
#' \itemize{
#'    \item the name of the regression noise filter.
#'    \item the parameters associated with the noise filter.
#'    \item the number of noisy and clean samples in the dataset.
#' }
#'
#' @param x an object of class \code{rfdata}.
#' @param ... other options to pass to the function.
#'
#' @return This function does not return any value.
#'
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # apply the regression noise filter
#' set.seed(9)
#' output <- regAENN(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # print the results
#' print(output)
#'
#' @seealso \code{\link{summary.rfdata}}, \code{\link{regAENN}}, \code{\link{regENN}}, \code{\link{regGE}}, \code{\link{regEF}}
#'
#' @export
print.rfdata <- function(x, ...){
  cat("\n## Noise filter: ", x$filter, sep="\n")

  if(!is.null(x$param)){
    cat("\n## Parameters:\n")
    for (i in 1:length(x$param)){
      cat("- ",attr(x$param,"names")[i]," = ",x$param[[i]],"\n",sep = "")
    }
  }

  cat("\n## Number of noisy and clean samples:\n")
  ncle <- x$numclean
  nnoi <- x$numnoise
  ntot <- ncle+nnoi
  cat("- Noisy samples: ", nnoi,"/",ntot," (",nnoi*100/ntot,"%)","\n",sep="")
  cat("- Clean samples: ", ncle,"/",ntot," (",ncle*100/ntot,"%)","\n",sep="")

}

###############################################################
###############################################################
###############################################################
#' Summary function for class rfdata
#'
#' This methods displays a summary containing information about the noise
#' filtering process contained in an \code{object} of class \code{rfdata}.
#'
#' This function presents a summary containing information of the regression noise filter and the resulting
#' dataset contained in the \code{object} of class \code{rfdata}.
#' The information offered is as follows:
#' \itemize{
#'    \item the function call.
#'    \item the name of the regression noise filter.
#'    \item the parameters associated with the noise filter.
#'    \item the number of noisy and clean samples in the dataset.
#'    \item the indices of the noisy and clean samples (if \code{showid = TRUE}).
#' }
#'
#' @param object an object of class \code{rfdata}.
#' @param showid a logical indicating if the indices of noisy samples must be displayed (default: \code{FALSE}).
#' @param ... other options to pass to the function.
#'
#' @return A list including information related to the noise filtering process contained in the object \code{object} of class {rfdata} with the following elements:
#'
#' \item{xclean}{a data frame with the input attributes of clean samples (without errors).}
#' \item{yclean}{a double vector with the output regressand of clean samples (without errors).}
#' \item{numclean}{an integer with the amount of clean samples.}
#' \item{idclean}{an integer vector with the indices of clean samples.}
#' \item{xnoise}{a data frame with the input attributes of noisy samples (with errors).}
#' \item{ynoise}{a double vector with the output regressand of noisy samples (with errors).}
#' \item{numnoise}{an integer with the amount of noisy samples.}
#' \item{idnoise}{an integer vector with the indices of noisy samples.}
#' \item{filter}{the full name of the noise filter used.}
#' \item{param}{a list of the argument values.}
#' \item{call}{the function call.}
#'
#' This list also includes the \code{showid} argument.
#'
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # apply the regression noise filter
#' set.seed(9)
#' output <- regAENN(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#'
#' # print the results
#' summary(output)
#'
#' @seealso  \code{\link{print.rfdata}}, \code{\link{regEF}}, \code{\link{regDF}}, \code{\link{regHRRF}}, \code{\link{regIRF}}
#'
#' @export
summary.rfdata <- function(object, ..., showid = FALSE){

  object <- structure(object, class = "sum.rfdata")
  object$showid <- showid
  return(object)
}

###############################################################
###############################################################
###############################################################
#' @export
print.sum.rfdata <- function(x, ...){

  cat("\n########################################################\n")
  cat("\tNoise filtering process: Summary\n")
  cat("########################################################\n\n")

  call <- deparse(x$call, getOption("width"))

  cat("## Original call:", call, sep = "\n")

  print.rfdata(x)

  if(x$showid){
    cat("\n## Indices of noisy samples:\n")
    id <- paste0(x$idnoise, collapse = ", ")
    if(id == "")
      id <- "-"
    cat(id, "\n", sep = "")
  }
}

###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
#' Plot function for class rfdata
#'
#' Graphical representation that allows for comparing data distributions before and after the noise filtering process.
#'
#' This function generates a plot for each of the variables specified by the \code{var} parameter,
#' allowing the comparison of their value distributions before filtering, using the descriptive statistic specified by \code{fun},
#' with the distributions of the data from samples identified as clean and noisy by the filtering method.
#'
#' @param x an object of \code{rfdata} class.
#' @param var an integer vector with the indices of variables whose distributions are compared, 
#' considering the attributes in the order in which they appear in the original data, with the output variable in the last position (default = \code{c(1)}).
#' @param fun a character containing the name of the descriptive statistic function to compute for each distribution of the variable, 
#' or a user-defined function that returns a value from a distribution of numeric values (default: \code{"mean"}). 
#' Some options for fun include "mean", "median" or "sd" (standard deviation).
#' @param ... other options to pass to the function.
#'
#' @return An object of class \code{ggplot} that graphically represents the data distributions before and after the noise filtering.
#'
#' @examples
#' # load the dataset
#' data(rock)
#'
#' # apply the regression noise filter
#' set.seed(9)
#' output <- regAENN(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])
#' 
#' # comparison chart of data distributions before and after the filtering process
#' plot(x = output, var = c(1:4), fun = "mean")
#'
#' @seealso \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
#' 
#' @export
#' @import "ggplot2"
plot.rfdata <- function(x, ..., var = c(1), fun = "mean"){
  
  ######### check for errors #########
  if(!inherits(x, "rfdata")){
    stop("argument \"x\" must be a class rfdata")
  }
  
  # Create the three datasets
  data_clean <- cbind(x$xclean, x$yclean)
  names(data_clean)[ncol(data_clean)] <- "output"
  
  data_noise <- cbind(x$xnoise, x$ynoise)
  names(data_noise) <- names(data_clean)
  
  data_ori <- rbind(data_clean, data_noise)
  names(data_ori) <- names(data_clean)
  
  # Get chosen attributes
  value_ori <- apply(X = data_ori[, var, drop = FALSE], MARGIN = 2, FUN = get(fun))
  value_clean <- apply(X = data_clean[, var, drop = FALSE], MARGIN = 2, FUN = get(fun))
  value_noise <- apply(X = data_noise[, var, drop = FALSE], MARGIN = 2, FUN = get(fun))
  
  # Create data.frame to plot
  d_val <- c(value_ori, value_clean, value_noise)
  d_var <- rep(names(data_ori)[var], times = 3)  
  d_dat <- rep(c("Original", "Clean", "Noisy"), each = length(var))
  dataplot <- data.frame(d_val, d_var, d_dat)
  
  order_categories <- c("Original", "Clean", "Noisy")
  dataplot$d_dat <- factor(dataplot$d_dat, levels = order_categories)
  dataplot$d_var <- factor(dataplot$d_var, levels = names(data_ori)[var])
  
  # Create final plot
  bar_plots <- ggplot(dataplot, aes(x = d_dat, y = d_val, fill = d_dat)) +
    geom_bar(stat = "identity") +
    facet_grid(d_var ~ ., scales = "free_y", switch = "y") +
    labs(x = "Dataset", y = "Variable") +
    theme_minimal() +
    theme(panel.spacing = unit(2, "lines"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 16), 
          axis.text = element_text(size = 10, face = "bold"), 
          axis.title = element_text(size = 10, face = "bold")) +
    scale_fill_manual(values = c("#4169E1", "#008000", "#FF0000"))
  
  bar_plots <- bar_plots + ggtitle(paste0("Noise filtering process - ", fun, " comparison"))
  
  return(bar_plots)
}
