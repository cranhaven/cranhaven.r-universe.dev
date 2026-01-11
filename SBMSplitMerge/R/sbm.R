#' Class \code{sbm}
#' @param blocks a \code{blocks} object
#' @param params a \code{params} object
#' @return an \code{sbm} object
#' @examples
#' sbm(blocks(c(1,1,2,2,3,3)), params(0.1, c(0.4,0.5,0.6)))
#' @export
sbm <- function(blocks, params){
    if(blocks$kappa != params$kappa)
        stop("Mis-matched number of blocks")
    colnames(params$thetak) <- names(blocks$n)
    structure(
        list(
            blocks = blocks
           ,
            params = params
           ,
            numnodes = blocks$numnodes
        )
       ,
        class = "sbm"
    )
}

#' @export
print.sbm <- function(x,...){
    cat("SBM object with:\n")
    print(x$blocks)
    print(x$params)
}


#' @title is.sbm
#' @description Logical check if an object is an \code{\link{sbm}} object
#' @param x an R object
#' @return Logical indicating if \code{x} is an \code{\link{sbm}} object
#' @export
is.sbm <- function(x){
    inherits(x, "sbm")
}

#' @importFrom grDevices rainbow
#' @title Plot for \code{\link{sbm}} object
#' @description plot an \code{\link{sbm}} object as an \code{image}
#' @param x an \code{\link{sbm}} object
#' @param col colours for each block - if missing, \code{rainbow} is used
#' @param ... additional arguments for plot
#' @return NULL
#' @seealso plot.default
#' @export
plot.sbm <- function(x, col, ...){
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
    graphics::par(mfrow=c(1,2))
    if(missing(col))
        col <- rainbow(x$blocks$kappa)
    plot(x$blocks, col=c(0, col), ...)
    plot(x$params, col=c(rep(1, x$params$dimtheta), rep(col, each=x$params$dimtheta)), ...)
}

#' @rdname plot.sbm
#' @export
image.sbm <- plot.sbm
