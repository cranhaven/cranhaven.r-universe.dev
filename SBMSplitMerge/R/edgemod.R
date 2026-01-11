#' @title Class for edge models
#' @description A class with a random and density method for \code{edges} objects
#' @param logd function(e, p) to calculate likelihood of edge an edge e given parameter array p
#' @param r function(p) - simulate an edge given a parameter p (optional)
#' @param ... additional arguments to append to \code{edgemod} internal list
#' @return an \code{edgemod} object
#' @note the parameter for \code{logd} is an array of c(dimension of theta, dim(E)) e.g. from \code{\link{parammat}}
#' @seealso \code{\link{edges_bern}} \code{\link{edges_pois}} \code{\link{edges_norm}}
#' @export
edgemod <- function(logd, r, ...){
    out <- list(logd=logd, ...)
    if(!missing(r))
        out$r <- r
    class(out) <- "edgemod"
    out
}

#' @export
print.edgemod <- function(x,...){
    cat("An edgemod object\n")
}

#' @title Bernoulli edge model
#' @description Make an \code{edgemod} model with Bernoulli edge-states
#' @param ... additional parameters to pass to \code{rbinom}
#' @return an \code{edgemod}
#' @examples
#' eb <- edges_bern() ## makes `eb` an edgemod for Bernoulli edge-states
#' @export
edges_bern <- function(...)
    edgemod(
        function(e, p, ...) stats::dbinom(e, 1, p, log=TRUE, ...)
       ,
        function(p, ...) stats::rbinom(1, 1, p, ...)
       ,
        name="bern"
       ,
        ...
    )

#' @title Poisson edge model
#' @description Make an \code{edgemod} model with Poisson edge-states
#' @param ... additional parameters to pass to \code{rpois}
#' @return an \code{edgemod}
#' @examples
#' ep <- edges_pois() ## makes `ep` an edgemod for Poisson edge-states
#' @export
edges_pois <- function(...)
    edgemod(
        function(e, p) stats::dpois(e, p, log=TRUE)
       ,
        function(p, ...) stats::rpois(1, p, ...)
       ,
        name="pois"
       ,
        ...
    )

#' @title Normal edge model
#' @description Make an \code{edgemod} model with Normal edge-states
#' @param ... additional parameters to pass to \code{rnorm}
#' @return an \code{edgemod}
#' @examples
#' en <- edges_norm() ## makes `en` an edgemod for Normal edge-states
#' @export
edges_norm <- function(...)
    edgemod(
        function(e, p, ...) stats::dnorm(e, p[1,,], p[2,,], log=TRUE, ...)
       ,
        function(p, ...) stats::rnorm(1, p[1], p[2], ...)
       ,
        name="norm"
       ,
        ...
    )

#' @title Negative-Binomial edge model
#' @description Make an \code{edgemod} model with Negative-Binomial edge-states
#' @param ... additional parameters to pass to \code{rnbinom}
#' @return an \code{edgemod}
#' @examples
#' enb <- edges_nbin() ## makes `enb` an edgemod for Negative-Binomial edge-states
#' @export
edges_nbin <- function(...)
    edgemod(
        function(e, p, ...) stats::dnbinom(e, p[1,,], p[2,,], log=TRUE, ...)
       ,
        function(p, ...) stats::rnbinom(1, p[1], p[2], ...)
       ,
        name="negative_binomial"
       ,
        ...
    )
