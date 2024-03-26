#' @name methods_m_surr
#' @title Method for class m_surr
#' @description A function to plots the m-surrounds give an object of the
#' class \emph{m_surr} obtain with the code \code{m.surround}.\cr
#'  The \code{plot()} function allows the user view the configuration of the m-surroundings.\cr
#'  The argument \code{type} select the type o visualization. \cr
#'  The \code{print()} print the matrix of the m-surrounding. \cr.
#'  The \code{summary} give information about the characteristics of the m-surroundings. \cr.
#'
#' @param x object of class \emph{m_surr}
#' @param type numeric. 1 (default) to get the plot with igraph.
#' @param object object of class \emph{m_surr}.
#' 2 plot W matrix with network
#' @param ... further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz, M., López, F., and Páez, A. (2021).
#'     A test for global and local homogeneity of categorical data based on spatial runs.
#'       \emph{Working paper}.
#'   }
#' @examples
#'
#' # Example 1: Obtain m-surroundings with degree of overlapping r
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' x <- cbind(cx,cy)
#' m = 4
#' r = 2
#' msurr_points <- m.surround(x = x, m = m, r = r,control = list(dtmaxabs = 0.5))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' print(msurr_points)
#'
#' # Example 2:
#' data("FastFood.sf")
#' m = 6
#' r = 1
#' msurr_points <-  m.surround(x = FastFood.sf, m = m, r = r, distance = "Euclidean",
#'                             control = list(dtmaxpc = .2))
#' plot(msurr_points, type = 1)
#' plot(msurr_points, type = 2)
#' print(msurr_points)
#'

NULL

#' @name summary.m_surr
#' @rdname methods_m_surr
#'
#' @export
summary.m_surr <- function(object, ...) {
  z <- object
  N <- z$N
  R <- z$R
  stopifnot(inherits(z, "m_surr"))
  no_symbolized <- (1:N)[!(1:N) %in% unique(matrix(z$ms,ncol = 1))]
  mh <- z$ms
  overlaping <- matrix(0,ncol = R, nrow = R)
  for ( i in 1:R){
    for (j in 1:R){
      overlaping[i,j] <- sum(mh[i,] %in% mh[j,])
    }
  }
  diag(overlaping) <-0

  # Print output
  cat("\nCharacteristics of m-surrounding:\n\n")
  cat(paste0("Number of m-surrounding (R): ",dim(z$ms)[1],"\n"))
  cat(paste0("Length of m-surrounding (m): ",dim(z$ms)[2],"\n"))
  cat(paste0("Number no-symbolized observations: ",length(no_symbolized),"\n"))
  cat("\nList of no-symbolized observations:\n")
  cat(no_symbolized)
  cat("\n\nList of the degree overlaping:\n")
  a <- table(rowSums(overlaping))
  mean.overlaping <- sum(as.numeric(names(a))*a)/sum(a)
  for (i in 1:length(a)){
    cat(paste0("    There are ",a[i], " m-surrounding that have intersection with ",names(a)[i]," m-surrounding","\n"))
  }
  cat(paste0("Mean degree of overlaping: ",round(mean.overlaping,4),"\n"))
}
#'

#' @name plot.m_surr
#' @rdname methods_m_surr
#'
#' @export
#'
plot.m_surr <- function(x, ..., type = 1){
  m_surr <- x
  if (dim(m_surr$ms)[1]==0){stop("The length of the m-sourronding is 0")}
  m <- dim(m_surr$ms)[2]
  W <- matrix(0, ncol =  m_surr$N, nrow =  m_surr$N)
  for (i in 1:dim(m_surr$ms)[1]){
    W[m_surr$ms[i,1], m_surr$ms[i,2:m]] <- 1
  }

  oldpar <- par(no.readonly = TRUE)

  if (type == 1){
    g1 <- graph.adjacency(W)
    list <- as.list(as.data.frame(t(m_surr$ms)))
    if (inherits(st_geometry(m_surr$x)[1],
        "sfc_MULTIPOLYGON"))
      lo <- layout.norm(st_coordinates(
        st_centroid(m_surr$x)))
    if (inherits(st_geometry(m_surr$x)[1],
                 "sfc_POLYGON"))
      lo <- layout.norm(
          st_coordinates(st_centroid(m_surr$x)))
    if (inherits(st_geometry(m_surr$x)[1],
                 "sfc_POINT"))
      lo <- layout.norm(st_coordinates(m_surr$x))
    mycolor <- as.matrix(0, ncol = 1, nrow = m_surr$N)
    mycolor <- "red"
    mycolor[m_surr$ms[,1]] = "black"
    plot(g1, margin = 0, edge.width=1,
         vertex.label.font = 1,
         vertex.label.cex = .5,
         vertex.label.dist = 1,
         mark.groups = list,
         edge.arrow.mode = 0, layout = lo, vertex.size = 3,
         vertex.color = mycolor, edge.color = 'black') # ,sub="black points are origin of m-surround"
    title(main = paste("m-surrounding;", "m = ", m_surr$m,
                       "and r = ", m_surr$r,"\n black points are origin of m-surrounding"),
          cex.main=1,col.main="black")
    on.exit(par(oldpar))
  }

  if (type == 2){
    W <- mat2listw(W)
    plot(st_geometry(m_surr$x))
    par(new = TRUE)
    on.exit(par(oldpar))
    if (inherits(st_geometry(m_surr$x)[1],
                 "sfc_MULTIPOLYGON"))
      plot(W, st_coordinates(
        st_centroid(m_surr$x)), col = "red")
    if (inherits(st_geometry(m_surr$x)[1],
                 "sfc_POLYGON"))
      plot(W, st_coordinates(
        st_centroid(m_surr$x)), col = "red")
    if (inherits(st_geometry(m_surr$x)[1],
        "sfc_POINT"))
      plot(W, st_coordinates(m_surr$x),
           col = "red")
    title(main = paste("m-surrounding;", "m = ", m_surr$m, "and r = ", m_surr$r))
  }
}

#' @name print.m_surr
#' @rdname methods_m_surr
#' @export
#'
print.m_surr <- function(x,...){
print(x$ms)
}



