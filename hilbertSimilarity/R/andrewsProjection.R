#' Use Andrews plots to visualize the Hilbert curve
#'
#' Use a Fourier series to project the Hilbert curve, based on the number of points
#' per Hilbert index. See \href{https://en.wikipedia.org/wiki/Andrews_plot}{Wikipedia - Andrews plot}
#' for a description of the method.
#'
#' @param x a matrix of counts, where rows correspond to samples and columns to Hilbert index
#' @param breaks the number of points used to display the Andrews curve
#' @return a list with 2 items:
#' \itemize{
#'     \item freq : a matrix with \code{breaks} rows and \code{ncol(x)} columns containing the Andrews vector for projection
#'     \item i : a vector with \code{breaks} elements corresponding to the Andrews indices
#' }
#' @details
#' The Andrews curve corresponds to a projection of each item to \eqn{(1/2^0.5,sin(t),cos(t),sin(2t),cos(2t),...)} where
#' \emph{t} (the Andrews index) varies between \eqn{-\pi} and \eqn{\pi}.
#'
#' @example examples/example.andrews.R
#'
#' @author Yann Abraham
#' @export
andrewsProjection <- function(x,breaks=30) {
    t <- seq(-pi, pi, length.out = breaks)
    n <- ncol(x)

    f <- matrix(t,nrow=length(t),ncol=n)
    j <- seq(2,n)

    f[,j[j%%2==0]] <- sweep(f[,j[j%%2==0],drop=FALSE],
                            2,
                            j[j%%2==0]/2,
                            `*`)

    f[,j[j%%2!=0]] <- sweep(f[,j[j%%2!=0],drop=FALSE],
                            2,
                            j[j%%2!=0]%/%2,
                            `*`)
    f[,j[j%%2==0]] <- sin(f[,j[j%%2==0]])
    f[,j[j%%2!=0]] <- cos(f[,j[j%%2!=0]])

    f[,1] <- 1/2^0.5

    return(list(freq=f,
                i=t))
}
