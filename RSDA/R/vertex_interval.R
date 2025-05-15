#' Vertex of the intervals
#' @name vertex.interval
#' @aliases vertex.interval
#' @author Jorge Arce.
#' @description Vertex of the intervals
#' @usage vertex.interval(sym.data)
#' @param sym.data Symbolic interval data table.
#'
#' @return Vertices of the intervals.
#' @references
#' Arce J. and Rodriguez O. (2015) 'Principal Curves and Surfaces
#' to Interval Valued Variables'. The 5th Workshop on Symbolic
#' Data Analysis, SDA2015, Orleans, France, November.
#'
#' Hastie,T. (1984).
#' Principal Curves and Surface. Ph.D Thesis Stanford University.
#'
#' Hastie,T. & Weingessel,A. (2014).
#' princurve - Fits a Principal Curve in Arbitrary Dimension.R package version 1.1--12
#' http://cran.r-project.org/web/packages/princurve/index.html.
#'
#' Hastie,T. & Stuetzle, W. (1989). Principal Curves.
#' Journal of the American Statistical Association, Vol. 84-406, 502--516.
#'
#' Hastie, T., Tibshirani, R. & Friedman, J. (2008).
#' The Elements of Statistical Learning; Data Mining, Inference and Prediction. Springer, New York.
#' @seealso sym.interval.pc
#' @keywords Principal Curve
#' @importFrom sqldf sqldf
#'
#'
#'
vertex.interval <- function(sym.data) {
  if ((sym.data$sym.var.types[1] != "$I")) {
    stop("Variables have to be continuos or Interval")
  } else {
    nn <- sym.data$N
    mm <- sym.data$M
    num.vertex <- rep(-1, nn)
    vertex <- matrix(0, 1, mm)
    vertex <- as.data.frame(vertex)
    colnames(vertex) <- sym.data$sym.var.names
    sym.text <- "as.matrix(sym.data$data["
    for (i in 1:nn) {
      current.row <- as.character(i)
      previous <- "1:2"
      command <- paste0(sym.text, current.row, ",", previous, "])")
      for (j in 2:mm) {
        col.current.min <- 2 * j - 1
        col.current.max <- 2 * j
        nxt.grid <- paste0(as.character(col.current.min), ":", as.character(col.current.max))
        command <- paste0(
          command, ",", sym.text, current.row, ",", nxt.grid,
          "])"
        )
      }
      command <- paste0("expand.grid(", command, ")")
      aux <- eval(parse(text = command))
      aux <- sqldf::sqldf("select distinct * from aux")
      num.vertex[i] <- dim(aux)[1]
      colnames(aux) <- sym.data$sym.var.names
      vertex <- rbind(vertex, aux)
    }
    num.vertexf <- dim(vertex)[1]
    return(list(vertex = vertex[2:num.vertexf, ], num.vertex = num.vertex))
  }
}
