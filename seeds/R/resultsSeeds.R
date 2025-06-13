#' Results Class for the Algorithms
#' 
#' A S4 class that collects the results of the two algorithms. The class also is equipped
#' with functions for easily plotting and extracting the different results.
#' 
#' @slot stateNominal data.frame containing the states of the nominal model
#' @slot stateEstimates data.frame containing the state estimates
#' @slot stateUnscertainLower lower bound of the estimated states as calculated by the baysian method
#' @slot stateUnscertainUpper upper bound of the estimated states as calculated by the baysian method
#' @slot hiddenInputEstimates estimated hidden input
#' @slot hiddenInputUncertainLower lower bounds of the estimated hidden inputs
#' @slot hiddenInputUncertainUpper upper bounds of the estimated hidden inputs
#' @slot outputEstimates estimated measurements resulting from the control of the hidden inputs
#' @slot outputEstimatesUncLower lower bound of the confidence bands of the estimated output
#' @slot outputEstimatesUncUpper upper bound of the confidence bands of the estimated output
#' @slot Data the given measurements
#' @slot DataError standard deviation of the given measurements
#' 
#' @return A object of class resultsSeeds collecting all the results of the algorithm
#' 
#' @export resultsSeeds
#' @exportClass resultsSeeds
#' 
#' @import methods
#' @importFrom graphics plot
resultsSeeds <- setClass(
  'resultsSeeds',
  slots = c(
    stateNominal = "data.frame",
    stateEstimates = "data.frame",
    stateUnscertainLower = "data.frame",
    stateUnscertainUpper = "data.frame",
    hiddenInputEstimates = "data.frame",
    hiddenInputUncertainLower = "data.frame",
    hiddenInputUncertainUpper = "data.frame",
    outputEstimates = "data.frame",
    outputEstimatesUncLower = "data.frame",
    outputEstimatesUncUpper = "data.frame",
    Data = "data.frame",
    DataError = "data.frame"
  ),
  prototype = c(
    stateNominal = data.frame(),
    stateEstimates = data.frame(),
    stateUnscertainLower = data.frame(),
    stateUnscertainUpper = data.frame(),
    hiddenInputEstimates = data.frame(),
    hiddenInputUncertainLower = data.frame(),
    hiddenInputUncertainUpper = data.frame(),
    outputEstimates = data.frame(),
    outputEstimatesUncLower = data.frame(),
    outputEstimatesUncUpper = data.frame(),
    Data = data.frame(),
    DataError = data.frame()
  )
)


plotResultsSeeds <- function(x, y) {

  seedsobj = x

  if (!missing(y)) {
    annoX <- y[[1]]
    annoY <- y[[2]]
    if (length(annoX) != length(names(seedsobj@stateEstimates[, -1]))) {
      stop('Number of names has to be be equal to the number of states')
    }

    if (length(annoY) != length(names(seedsobj@outputEstimates[, -1]))) {
      stop('Length of measurement annotations character vector has to be equal to the number of states')
    }

  } else {
    annoX <- names(seedsobj@stateEstimates[, -1])
    annoY <- names(seedsobj@outputEstimates[, -1])
  }

  labelX <- function(ls, val) {
    dic <- list(annoX)
    return(dic[val])
  }

  labelY <- function(ls, val) {
    dic <- list(annoY)
    return(dic[val])
  }


  # added formating for plotting the states in the right order
  reformatOrder <- function(df, annoT) {
    df$facet = factor(df$state, levels = as.character(unique(factor(df$state))))

    return(df)
  }


  smoothRes <- function(df) {
    omitNan <- df[, !is.nan(colSums(df)), drop = FALSE]
    df[, !is.nan(colSums(df))] = apply(X = omitNan, MARGIN = 2, FUN = function(x) stats::smooth(x = x))
    return(df)
  }

  line_width <- 0.75
  
  state <- NULL
  value <- NULL
  value.y <- NULL
  value.x <- NULL
  

  plot1 <- ggplot2::ggplot(reformatOrder(tidyr::gather(smoothRes(seedsobj@stateEstimates), state, value, -1)), ggplot2::aes(x = t, y = value, colour = 'red')) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_line(data = reformatOrder(tidyr::gather(smoothRes(seedsobj@stateNominal), state, value, -1)), ggplot2::aes(x = t, y = value, colour = 'blue'), size = line_width) +
    ggplot2::geom_errorbar(data = reformatOrder(dplyr::inner_join(tidyr::gather(smoothRes(seedsobj@stateUnscertainUpper), state, value, -1), tidyr::gather(smoothRes(seedsobj@stateUnscertainLower), state, value, -1), by = c("t", "state"))), ggplot2::aes(x = t, ymin = value.y, ymax = value.x), alpha = 0.2, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::labs(x = 't', y = 'value', color = "states") +
    ggplot2::scale_color_manual(breaks = c("red", "blue"), labels = c("estimate", "nominal"), values = c("blue", "red")) +
    ggplot2::theme(legend.position = "none",
                  strip.background = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                  panel.grid.major = ggplot2::element_blank()) +
    ggplot2::facet_wrap(~facet, labeller = labelX)



  plot2 <- ggplot2::ggplot(data = reformatOrder(tidyr::gather(smoothRes(seedsobj@hiddenInputEstimates), state, value, -1)), ggplot2::aes(x = t, y = value, colour = "red")) +
    ggplot2::geom_line(size = line_width) +
    ggplot2::geom_errorbar(data = reformatOrder(dplyr::inner_join(tidyr::gather(smoothRes(seedsobj@hiddenInputUncertainUpper), state, value, -1), tidyr::gather(smoothRes(seedsobj@hiddenInputUncertainLower), state, value, -1), by = c("t", "state"))), ggplot2::aes(x = t, ymin = value.y, ymax = value.x), alpha = 0.2, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::theme(legend.position = "none",
                  strip.background = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                  panel.grid.major = ggplot2::element_blank()) +
    ggplot2::facet_wrap(~facet)

  plot3 <- ggplot2::ggplot(data = reformatOrder(tidyr::gather(smoothRes(seedsobj@outputEstimates), state, value, -1)), ggplot2::aes(x = t, y = value, colour = state)) +
    ggplot2::geom_line(size = line_width) +
    ggplot2::geom_errorbar(data = reformatOrder(dplyr::inner_join(tidyr::gather(smoothRes(seedsobj@outputEstimatesUncUpper), state, value, -1), tidyr::gather(smoothRes(seedsobj@outputEstimatesUncLower), state, value, -1), by = c("t", "state"))), ggplot2::aes(x = t, ymin = value.y, ymax = value.x), alpha = 0.2, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::geom_errorbar(data = reformatOrder(dplyr::inner_join(tidyr::gather(seedsobj@Data, state, value, -1), tidyr::gather(seedsobj@DataError, state, value, -1), by = c("t", "state"))), ggplot2::aes(x = t, ymin = value.x - value.y, ymax = value.x + value.y), inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::geom_point(data = reformatOrder(tidyr::gather(seedsobj@Data, state, value, -1)), ggplot2::aes(x = t, y = value), colour = "black") +
    ggplot2::labs(x = 't', y = 'value', color = "Estimated \n measurements") +
    ggplot2::scale_color_manual(labels = labels(seedsobj@outputEstimates[, -1])[[2]], values = rep("red", length(labels(seedsobj@outputEstimates[, -1])[[2]]))) +
    ggplot2::theme(legend.position = "none",
                  strip.background = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                  panel.grid.major = ggplot2::element_blank()) +
    ggplot2::facet_wrap(~facet, drop = TRUE, labeller = labelY)

  return(list(plot1, plot2, plot3))

}

printSeedsResults <- function(x) {
  cat('Number of observations', ncol(x@Data) - 1, '\n')
  cat('Number of states', ncol(x@stateEstimates) - 1, '\n')
  cat('Mean deviation states: ')
  cat(colMeans(x@DataError[, -1]), '\n\n')
  cat('Estimated states interpolated by given measuerment points\n')

  tOut <- x@Data[, 1]
  tIn <- x@stateEstimates[, 1]
  interpStates <- apply(X = x@stateEstimates[, -1], MARGIN = 2, FUN = function(x) stats::approx(x = tIn, y = x, tOut, rule = 2, method = 'linear'))
  interpStatesDf <- do.call(cbind, lapply(interpStates, FUN = function(x) cbind(x$y)))
  interpStatesDf = cbind(tOut, interpStatesDf)
  colnames(interpStatesDf) <- colnames(x@stateEstimates)
  print(interpStatesDf)
}

#### printing function ####

#' A default printing function for the resultsSeeds class
#'
#' This function overwrites the default print function and is used for objects 
#' of the class resultsSeeds. The print function gives the basic information about
#' the results seeds object. The default printout is the estimated states and 
#' the calculated hidden inputs
#' 
#' @param x an object of the class resultsSeeds
#' 
#' @return Returns a short summary of the important results
#'
#' @aliases print,resultsSeeds
#' 
#' @examples
#' data(ubv_res)
#' 
#' plot(res[[2]])
#' 
#'
#' @export
#' 
#' @rdname print-seeds
#' 

setMethod(f = 'print',
          signature = 'resultsSeeds',
          definition = function(x) {
            printSeedsResults(x)
          }
)



#### plotting function ####

#' Plot method for the S4 class resultsSeeds
#' 
#' A standardized plot function to display the results of the algorithms. Both
#' algorithms should result in objects of the class resultsSeeds. The results can
#' be plotted using the \code{\link{plot}}-function.
#' 
#' @param x an object of type resultsSeeds or a list of these objects. If a list
#' is given the last entry will be plotted.
#' @param y ...
#' 
#' @return A list of plots showing the results of the algorithm
#' 
#' @aliases plot,resultsSeeds,missing-method
#' 
#' @examples 
#' 
#' data(uvb_res)
#' 
#' plot(res[[2]])
#' 
#' @export

#' @rdname plotseeds
setMethod(f = "plot",
          signature = c(x = "resultsSeeds", y = "missing"),
          definition = function(x, y) {

            plotList <- plotResultsSeeds(x, y)

            return(plotList)
          }
)




#' Create annotated plot
#' 
#' Create a annotated plot with given state and measurement names. The plots are
#' equal to the output of the normal plot function.
#' @param x an object of type resultsSeeds which contains the results of the algorithms
#' @param stateAnno a character vector describing the names of the states
#' @param measAnno a character vector describing the names of the measurements
#' 
#' @return Plots of the results with the provided annotation
#' 
#' @examples 
#' 
#' data(uvb_res)
#' 
#' statesAnno <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")
#' measurAnno <- c("y1", "y2", "y3", "y4", "y5")
#' 
#' plotAnno(res[[2]], stateAnno = statesAnno, measAnno =  measurAnno)
#' 
#' @export
setGeneric(name = "plotAnno", function(x, stateAnno, measAnno) standardGeneric("plotAnno"))

#' @rdname plotAnno
setMethod(f = "plotAnno",
          signature = "resultsSeeds",
          definition = function(x, stateAnno, measAnno) {
            y <- list(stateAnno, measAnno)
            plotList <- plotResultsSeeds(x, y)

            return(plotList)
          }
)

#' @rdname plotAnno
setMethod(f = "plotAnno",
          signature = "list",
          definition = function(x, stateAnno, measAnno) {
            x <- x[[length(x)]]
            y <- list(stateAnno, measAnno)
            plotList <- plotResultsSeeds(x, y)

            return(plotList)
          }
)


#### return hidden Inputs ####
#' Get the estimated hidden inputs
#' 
#' @param resultsSeeds A object of the class 'resultsSeeds', which is returned from the algorithms.
#' @param ind A numeric indicating the index of a 'resultsSeeds'-Object in a list. If not set the last listed object will be used.
#'
#' @return Dataframe containing the estimated hidden inputs
#' 
#' @examples 
#' 
#' data(uvb_res)
#' 
#' hiddenInputs(res[[2]])
#' 
#' @export

setGeneric(name = "hiddenInputs",
           def = function(resultsSeeds, ind) {
            standardGeneric("hiddenInputs")
           }
)

#' @rdname hiddenInputs
setMethod(f = "hiddenInputs",
          signature = c("list", "numeric"),
          definition = function(resultsSeeds, ind) {
            return(resultsSeeds[[ind]]@hiddenInputEstimates)
          }
)

#' @rdname hiddenInputs
setMethod(f = "hiddenInputs",
          signature = c("list", "missing"),
          definition = function(resultsSeeds, ind) {
            ind <- length(resultsSeeds)
            return(resultsSeeds[[ind]]@hiddenInputEstimates)
          }
)

#' @rdname hiddenInputs
setMethod(f = "hiddenInputs",
          signature = c("resultsSeeds", "missing"),
          definition = function(resultsSeeds, ind) {
            return(resultsSeeds@hiddenInputEstimates)
          }
)

#### return estimated states ####
#' Get the estimated states
#' 
#' @param resultsSeeds A object of the class resultsSeeds, which is returned from the algorithms.
#' @param ind A numeric indicating the index of a resultsSeeds-Object in a list. If not set the last listed object will be used.
#'
#' @return Dataframe containing the estimated states
#' 
#' @examples 
#' 
#' data(uvb_res)
#' 
#' estiStates(res)
#' 
#' @export
setGeneric(name = "estiStates",
           def = function(resultsSeeds, ind) {
            standardGeneric("estiStates")
           }
)

#' @rdname estiStates
setMethod(f = "estiStates",
          signature = c("list", "numeric"),
          definition = function(resultsSeeds, ind) {
            return(resultsSeeds[[ind]]@stateEstimates)
          }
)

#' @rdname estiStates
setMethod(f = "estiStates",
          signature = c("list", "missing"),
          definition = function(resultsSeeds, ind) {
            ind <- length(resultsSeeds)
            return(resultsSeeds[[ind]]@stateEstimates)
          }
)

#' @rdname estiStates
setMethod(f = "estiStates",
          signature = c("resultsSeeds", "missing"),
          definition = function(resultsSeeds, ind) {
            return(resultsSeeds@stateEstimates)
          }
)

#### return estimated outputs ####
#' Get the estimated outputs
#' 
#' @param resultsSeeds A object of the class 'resultsSeeds', which is returned from the algorithms.
#' @param ind A numeric indicating the index of a 'resultsSeeds'-Object in a list. If not set the last listed object will be used.
#'
#' @return Dafaframe with estimated measurements.
#' 
#' @examples 
#' 
#' data(uvb_res)
#' 
#' outputEstimates(res[[2]])
#' 
#' @export
setGeneric(name = "outputEstimates",
           def = function(resultsSeeds, ind) {
            standardGeneric("outputEstimates")
           }
)

#' @rdname outputEstimates
setMethod(f = "outputEstimates",
          signature = c("list", "numeric"),
          definition = function(resultsSeeds, ind) {
            return(resultsSeeds[[ind]]@outputEstimates)
          }
)

#' @rdname outputEstimates
setMethod(f = "outputEstimates",
          signature = c("list", "missing"),
          definition = function(resultsSeeds, ind) {
            ind <- length(resultsSeeds)
            return(resultsSeeds[[ind]]@outputEstimates)
          }
)

#' @rdname outputEstimates
setMethod(f = "outputEstimates",
          signature = c("resultsSeeds", "missing"),
          definition = function(resultsSeeds, ind) {
            return(resultsSeeds@outputEstimates)
          }
)

#### return confidence bands ####
#' Get the estimated confidence bands for the bayesian method
#' 
#' @param resultsSeeds A object of the class resultsSeeds, which is returned from the algorithms.
#' @param slot Specifies the slot. Options are "states", "hiddenInputs", "outputs"
#' @param ind A numeric indicating the index of a resultsSeeds-Object in a list. If not set the last listed object will be used.
#'
#' @return A dataframe containing the confidence bands of the estiamted states, hidden inputs and outputs
#' 
#' @examples 
#' 
#' data(uvb_res)
#' 
#' confidenceBands(res, slot = "states", ind = 2) 
#' 
#' @export
setGeneric(name = "confidenceBands",
           def = function(resultsSeeds, slot, ind) {
            standardGeneric("confidenceBands")
           }
)

#' @rdname confidenceBands
setMethod(f = "confidenceBands",
          signature = c("list", "character", "numeric"),
          definition = function(resultsSeeds, slot, ind) {
            res <- list()
            if (slot == "states") {
              res$lower <- resultsSeeds[[ind]]@stateUnscertainLower
              res$upper <- resultsSeeds[[ind]]@stateUnscertainUpper
            }
            if (slot == "hiddenInputs") {
              res$lower <- resultsSeeds[[ind]]@hiddenInputUncertainLower
              res$upper <- resultsSeeds[[ind]]@hiddenInputUncertainUpper
            }
            if (slot == "output") {
              res$lower <- resultsSeeds[[ind]]@outputEstimatesUncLower
              res$upper <- resultsSeeds[[ind]]@outputEstimatesUncUpper
            }
            return(res)
          }
)

#' @rdname confidenceBands
setMethod(f = "confidenceBands",
          signature = c("list", "character", "missing"),
          definition = function(resultsSeeds, slot, ind) {
            ind <- length(resultsSeeds)
            res <- list()
            if (slot == "states") {
              res$lower <- resultsSeeds[[ind]]@stateUnscertainLower
              res$upper <- resultsSeeds[[ind]]@stateUnscertainUpper
            }
            if (slot == "hiddenInputs") {
              res$lower <- resultsSeeds[[ind]]@hiddenInputUncertainLower
              res$upper <- resultsSeeds[[ind]]@hiddenInputUncertainUpper
            }
            if (slot == "output") {
              res$lower <- resultsSeeds[[ind]]@outputEstimatesUncLower
              res$upper <- resultsSeeds[[ind]]@outputEstimatesUncUpper
            }
            return(res)
          }
)

#' @rdname confidenceBands
setMethod(f = "confidenceBands",
          signature = c("resultsSeeds", "character", "missing"),
          definition = function(resultsSeeds, slot, ind) {
            ind <- length(resultsSeeds)
            res <- list()
            if (slot == "states") {
              res$lower <- resultsSeeds@stateUnscertainLower
              res$upper <- resultsSeeds@stateUnscertainUpper
            }
            if (slot == "hiddenInputs") {
              res$lower <- resultsSeeds@hiddenInputUncertainLower
              res$upper <- resultsSeeds@hiddenInputUncertainUpper
            }
            if (slot == "output") {
              res$lower <- resultsSeeds@outputEstimatesUncLower
              res$upper <- resultsSeeds@outputEstimatesUncUpper
            }
            return(res)
          }
)

#### summary #####
summary.resultsSeeds <- function(resultsSeeds) {

  states <- list()
  hidInputs <- list()

  states$min <- apply(X = resultsSeeds@stateEstimates, MARGIN = 2, FUN = function(x) min(x))
  hidInputs$min <- apply(X = resultsSeeds@hiddenInputEstimates, MARGIN = 2, FUN = function(x) min(x))

  states$q1 <- apply(X = resultsSeeds@stateEstimates, MARGIN = 2, FUN = function(x) stats::quantile(x, 0.25))
  hidInputs$q1 <- apply(X = resultsSeeds@hiddenInputEstimates, MARGIN = 2, FUN = function(x) stats::quantile(x, 0.25))

  states$mean <- apply(X = resultsSeeds@stateEstimates, MARGIN = 2, FUN = function(x) mean(x))
  hidInputs$mean <- apply(X = resultsSeeds@hiddenInputEstimates, MARGIN = 2, FUN = function(x) mean(x))

  states$median <- apply(X = resultsSeeds@stateEstimates, MARGIN = 2, FUN = function(x) stats::median(x))
  hidInputs$median <- apply(X = resultsSeeds@hiddenInputEstimates, MARGIN = 2, FUN = function(x) stats::median(x))

  states$q3 <- apply(X = resultsSeeds@stateEstimates, MARGIN = 2, FUN = function(x) stats::quantile(x, 0.75))
  hidInputs$q3 <- apply(X = resultsSeeds@hiddenInputEstimates, MARGIN = 2, FUN = function(x) stats::quantile(x, 0.75))

  states$max <- apply(X = resultsSeeds@stateEstimates, MARGIN = 2, FUN = function(x) max(x))
  hidInputs$max <- apply(X = resultsSeeds@hiddenInputEstimates, MARGIN = 2, FUN = function(x) max(x))

  states <- do.call(what = rbind, states)
  row.names(states) <- c('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max')
  hiddenInputs <- do.call(what = rbind, hidInputs)
  row.names(hiddenInputs) <- c('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max')

  return(list("est. states" = states, "est. hiddenInputs" = hiddenInputs))
}

