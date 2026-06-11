globalVariables(c("ID", "time2"))

#' @name PanelSurv
#' @rdname PanelSurv
#' @title Create a PanelSurv Object
#'
#' @description Create a panel count survival object,
#' usually used as a response variable in a model formula.
#'
#' @param ID Observation subject's ID.
#' @param time Observation time.
#' @param count Observation subject's ID.
#' @param x An \code{PanelSurv} object.
#'
#' @return An object of S3 class \code{"PanelSurv"}.
#' \describe{
#'   \item{psDF}{a data frame, part of original input data frame with variable
#' "ID", "time" and "count".}
#'   \item{timeGrid}{ordered distinct observation times in the set of all
#'     observation times.}
#'   \item{panelMatrix}{a matrix representation of panel count data, one
#'     row per subject, one column per time point in \code{"timeGrid"}.}
#' }
#'
#'   In the case of \code{is.PanelSurv}, a logical value \code{TRUE} if
#'   \code{x} inherits from class \code{"PanelSurv"}, otherwise an \code{FALSE}.
#'
#'   In the case of \code{plot.PanelSurv}, a tile plot of
#'   \code{panelMatrix} produced by package \code{ggplot2} with color
#' indicating number of counts since last observation time.
#'
#' @seealso \code{\link{panelReg}}
#' @example inst/examples/ex_PanelSurv.R
NULL

#' @rdname PanelSurv
#' @export
PanelSurv <- function(ID, time, count) {
    if (sum(time <= 0) > 0)
        stop("Observation time must be positive.")
    index <- which(!duplicated(ID))
    N <- length(index)
    uniqueID <- ID[index]
    timeGrid <- sort(unique(time))
    panelMatrix <- matrix(NA, N, length(timeGrid))
    for (i in 1:N) {
        rowSet <- which(ID == uniqueID[i])
        panelMatrix[i, which(timeGrid %in% time[rowSet])] <- count[rowSet]
    }
    ps <- list(psDF=data.frame(ID = ID, time=time, count=count),
               timeGrid=timeGrid, panelMatrix=panelMatrix)
    class(ps) <- "PanelSurv"
    ps
}

#' @rdname PanelSurv
#' @export
is.PanelSurv <- function(x) inherits(x, "PanelSurv")

#' Produce Tile Plot
#'
#' Plot the tile plot from a \code{PanelSurv} object.
#'
#' @param x an object of class \code{PanelSurv}.
#' @param heat an optional logical value indicating whether
#' a swimmer-plot-like tile plot will be produced.
#' @param order an optional logical value indicating whether the tile plot
#' will be sorted by the largest observationt time.
#' @param ... future extension
#'
#' @importFrom ggplot2 ylab xlab geom_bar coord_flip theme element_blank element_text
#' @return A \code{ggplot} object
#' @export
plot.PanelSurv <- function(x, heat = FALSE, order = TRUE, ...) {
    tmp <- aggregate(time ~ ID, length, data = x$psDF)[,2]
    x$psDF$ID <- rep(1:length(tmp), tmp)
    if (order) {
        ranks <- rank(aggregate(time ~ ID, max, data = x$psDF)$time, ties.method = "first")
        x$psDF$ID <- ranks[x$psDF$ID]
        x$psDF <- x$psDF[order(x$psDF$ID),]
    }
    if (!heat) {
        tileH <- 4500 / length(unique(x$psDF$ID))
        ggplot(data = x$psDF, aes(time, ID, height = 2, width = tileH)) +
            geom_tile(aes(fill = count)) + theme_bw() +
            scale_fill_gradient(low = "grey", high = "black") +
            ylab("Time") + xlab("Subject")
    } else {
        x$psDF$time2 <- with(x$ps, unlist(lapply(split(time, ID), function(x) diff(c(0, x)))))
        ggplot(x$psDF, aes(ID, time2, fill = count)) +
            geom_bar(stat = "identity", color = "black", size = .1) +
            scale_fill_gradient(low = "gray80", high = "gray10") +
            theme_bw() + coord_flip() +
            theme(axis.line.y = element_blank(),
                  axis.title.y = element_text(vjust = 0),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()) +
            ylab("Time") + xlab("Subject")
    }
}
