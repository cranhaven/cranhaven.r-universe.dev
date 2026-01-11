#' @title Stochastic block model object
#' @description A wrapper for a block and parameter model
#' @details Simple wrapper for the block and parameter model for an \code{sbm} object
#' @param blockmod a \code{\link{blockmod}} object
#' @param parammod a \code{\link{parammod}} object
#' @param edgemod an \code{\link{edgemod}} object
#' @param ... additional arguments to store in the \code{sbmmod} object
#' @seealso \code{\link{blockmod}} \code{\link{parammod}} \code{\link{edgemod}}
#' @return an \code{sbmmod} object with a method `r(n)` sampling an \code{sbm} object with \code{n} nodes from the model and a method \code{logd(sbm)} computing the log-density of \code{sbm} under the model
#' @export
#' @author Matthew Ludkin
sbmmod <- function(blockmod, parammod, edgemod, ...){
    structure(
        list(
            logd = function(sbm, edges, ...){
                dedges(sbm, edges, edgemod, ...) +
                    blockmod$logd(sbm$blocks) +
                    parammod$logd(sbm$params)
            }
           ,
            r = function(n, sorted=FALSE){
                b <- blockmod$r(n, sorted=sorted)
                p <- parammod$r(b$kappa)
                sbm(b, p)
            }
           ,
            simedges = function(sbm, sym=TRUE, loops=FALSE){
                redges(sbm, edgemod, sym=sym, loops=loops)
            }
           ,
            block=blockmod, param=parammod, edge=edgemod
           ,
            ...
        )
       ,
        class = "sbmmod"
    )
}

#' @export
print.sbmmod <- function(x, ...){
    cat("an sbmmod object with block model:")
    print(x$block)
    cat("param model:")
    print(x$param)
    cat("edge model:")
    print(x$edge)
}
