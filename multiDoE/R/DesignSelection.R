#### optMultiCrit ####
#' Selection of a symmetrical design on the Pareto Front based on the utopian point
#'
#'
#' @description The \code{optMultiCrit} function provides an objective criterion
#' for the selection of the best experimental design among all Pareto front solutions.
#' The selection is based on minimizing the euclidean distance in the criteria space
#' between all the Pareto front points and an approximate utopian point. By default,
#' the coordinates of the utopian point correspond to the minimum value reached
#' by each criterion during the \code{\link[multiDoE]{runTPLS}} optimization procedure.
#' Alternatively, the utopian point can be chosen by the user.
#'
#' @param ar A list as the \code{megaAR} list returned by \code{\link[multiDoE]{runTPLS}}.
#' @param ... optional argument (see below).
#'
#' @details Additional arguments can be specified as follows:
#' \itemize{
#' \item \code{myUtopianPoint}: A vector containing the utopian point coordinates.
#' }
#'
#' @return The \code{optMultiCrit} function returns a list whose elements are:
#' \itemize{
#' \item \code{solution}: The selected optimal design matrix.
#' \item \code{score}: A vector containing the criteria scores for \code{solution}.
#' }
#'
#'
#' @importFrom stats dist
#'
#' @export
optMultiCrit <- function(ar, ...) {
  varargin <- list(...)

  if (nargs() == 1) {
    bestPoint <- apply(ar$scores, 2, min)
  } else {
    bestPoint <- varargin[[1]]
  }

  d <- c()
  for (i in 1:ar$nsols) {
    d[i] <- dist(rbind(ar$scores[i, ], bestPoint))
  }

  ind <- which(d == min(d))
  return(list("solution" = ar$solutions[ind], "scores" = ar$score[ind, ]))
}

#### optSingleCrit ####
#' Selection of the best design from the Pareto Front for each criterion
#'
#' @description The \code{optSingleCrit} function selects from the Pareto front
#' those designs that minimize the criteria when considered individually.
#'
#' @param ar A list as the \code{megaAR} list returned by \code{\link[multiDoE]{runTPLS}}.
#'
#' @return A list whose \eqn{i}-th element corresponds to the solution that optimizes
#' the \eqn{i}-th criterion. Every solution is a list of two elements:
#' \itemize{
#' \item \code{score}: Scores vector.
#' \item \code{solution}: The design matrix.
#' }
#' @export
#'
optSingleCrit <- function(ar) {

  nCrit <- dim(ar$scores)[2]
  best <- vector("list", nCrit)
  index <- apply(ar$scores, 2, which.min)

  for (i in 1:nCrit) {
    best[[i]] <- list( scores = ar$scores[index[i], ], solution = ar$solutions[[index[i]]])
  }

  names(best) <- colnames(ar$scores)
  return(best)
}

#### plotPareto ####
#' Graphical representation of the Pareto Front
#'
#' @description \code{plotPareto} returns a graphical representation (at most 3D)
#' of the Pareto front.
#'
#' @usage plotPareto(ar, x, y, z = NULL, mode = TRUE)
#'
#' @param ar A list as the \code{megaAR} list returned by \code{\link[multiDoE]{runTPLS}}.
#' @param x The criterion on the x axis. It can be one of the following: \code{"I",
#' "Id", "D", "Ds", "A"} and \code{"As"}.
#' @param y The criterion on the y axis. It can be one of the following: \code{"I",
#' "Id", "D", "Ds", "A"} and \code{"As"}.
#' @param z The criterion on the z axis. It can be one of the following: \code{"I",
#' "Id", "D", "Ds", "A"} and \code{"As"}.
#' @param mode When \code{mode=True} the function returns a 3D interactive
#' chart. When \code{mode=False} it returns a 2D chart in which the \code{z} criteria
#' values are represented by a color scale.
#'
#' @return The Pareto front chart.
#'
#' @import ggplot2
#' @importFrom plotly plot_ly layout
#'
#' @export
plotPareto <- function(ar, x, y, z = NULL, mode = TRUE){

  # data.frame
  if (ar$nsols == 1) {
    nCrit <- length(ar$scores)
    df <- as.data.frame(t(ar$scores))
  } else {
    nCrit <- dim(ar$scores)[2]
    df <- as.data.frame(ar$scores)
  }

  # 2d
  if (is.null(z)) {
    ggplot(df, aes_string(x = colnames(df)[1], y = colnames(df)[2])) + geom_point() +
      scale_x_continuous(n.breaks = 10) +
      scale_y_continuous(n.breaks = 10)
  } else if (is.null(z) == F & mode == T) {   # 3d interactive

    fig <- plot_ly(data = df, type="scatter3d",mode='markers',x = ~ df[[x]],
                   y = ~ df[[y]], z = ~ df[[z]],hoverinfo = 'text',
                   marker = list(size = 5),
                   text = ~paste(" ",x,": ", round(df[[x]], 6), '<br>', y, ": ",
                                 round(df[[y]], 6), '<br>', z, ": ", round(df[[z]], 6)))
    fig <- fig %>% layout(scene = list(xaxis = list(title = x),
                                       yaxis = list(title = y),
                                       zaxis = list(title = z)))
    fig

  } else if (is.null(z) == F & mode == FALSE) {   # 3d = 2d + color
    ggplot(data = df, mapping = aes_string(x = x, y = y)) +
      geom_point(aes_string(colour = z), shape = 19) +
      scale_x_continuous(n.breaks = 6) +
      scale_y_continuous(n.breaks = 8)
  } else {
    stop("Number of criteria not valid")
  }
}





