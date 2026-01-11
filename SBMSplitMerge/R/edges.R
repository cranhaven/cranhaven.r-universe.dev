#' @title Class for edge data
#' @description A class to hold edge data
#' @param e a matrix or array representing the raw edge-state data
#' @param sym is the network symmetric? (\code{e[ji] = e[ji]})
#' @param loops does the network contain self-loops? (edges from node i to i)
#' @param ... additional arguments to append to edges internal list
#' @return an edges object
#' @export
#' @examples
#' ## make an sbm model, sample data then plot and print:
#' model <- sbmmod(dma(2,5), param_beta(1,1,1,1), edges_bern())
#' s <- model$r(100)
#' e <- redges(s, model$edge)
#' plot(e)
#' plot(e, s)
#' print(e)
edges <- function(e, sym, loops, ...){
    if(missing(sym))
        sym <- all(e==t(e))
    if(missing(loops))
        loops <- !all(diag(e)==0)
    structure(
        list(
            E = e
           ,
            numnodes = ncol(e)
           ,
            sym = sym
           ,
            loops = loops
           ,
            ...
        )
       ,
        class = "edges"
    )
}

#' @export
print.edges <- function(x, ...)
    print(paste("edges object on", x$numnodes, "nodes"))

#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot .data geom_raster theme scale_alpha xlab ylab aes element_blank
#' @title Plot
#' @description plots an \code{\link{edges}} objects
#' @param x an \code{\link{edges}} object
#' @param Blocks a blocks object or \code{\link{sbm}} object
#' @param sorted sort by block membership in \code{\link{sbm}} before plotting?
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param ... parameters for \code{image}
#' @return \code{ggplot2} plot of edges in a raster
#' @export
#' @examples
#' ## make an sbm model, sample data then plot and print:
#' model <- sbmmod(dma(2,5), param_beta(1,1,1,1), edges_bern())
#' s <- model$r(100)
#' e <- redges(s, model$edge)
#' plot(e)
#' plot(e, s)
#' print(e)
plot.edges <- function(x, Blocks, sorted=TRUE, xlab="Node", ylab="Node", ...){
    ord <- 1:x$numnodes
    if(sorted & !missing(Blocks)){
        if(is.sbm(Blocks))
            Blocks <- Blocks$blocks
        ord <- order(Blocks$z)
        Blocks$z <- Blocks$z[ord]
    }
    colnames(x$E) <- rownames(x$E) <- NULL
    df <- reshape2::melt(x$E[ord,ord])
    if(!missing(Blocks)){
        df$block1 <- 0
        df$block2 <- 0
        for(i in 1:x$numnodes)
            df$block2[df$Var2 == i] <- df$block1[df$Var1 == i] <- as.numeric(as.character(Blocks$z[i]))
        df$block <- df$block1 * (df$block1 == df$block2)
    }
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$Var1, .data$Var2)) +
        ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
              panel.grid.major=ggplot2::element_blank()) +
        ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    if(!missing(Blocks)){
        p <- p + ggplot2::geom_raster(ggplot2::aes(fill=factor(.data$block)))
    }
    p <- p + ggplot2::geom_raster(ggplot2::aes(alpha=as.numeric(.data$value))) +
        ggplot2::scale_alpha(range=c(0, 0.4), name="value")
    p
}

#' @rdname plot.edges
#' @export
image.edges <- plot.edges

#' @title Simulate edges
#' @description Simulate edges from an \code{\link{sbm}} object with a given \code{\link{edgemod}}
#' @details None
#' @param SBM an \code{\link{sbm}} object
#' @param edgemod an \code{\link{edgemod}} object
#' @param sym should the network be symmetric?
#' @param loops should the network have self-loops?
#' @param ... additional arguments passed to \code{edgemod$r}
#' @return an \code{edges} object
#' @export
#' @examples
#' ## make an sbm model, sample data then plot and print:
#' model <- sbmmod(dma(2,5), param_beta(1,1,1,1), edges_bern())
#' s <- model$r(100)
#' e <- redges(s, model$edge)
#' plot(e)
#' plot(e, s)
#' print(e)
redges <- function(SBM, edgemod, sym=TRUE, loops=FALSE, ...){
    if(is.null(edgemod$r))
        stop("The edgemod does not specify a random method via $r so can't simulate edges")
    pmat <- parammat(SBM)
    e <- apply(pmat, 2:3, edgemod$r, ...=...)
    if(sym)
        e[lower.tri(e)] <- t(e)[lower.tri(e)]
    if(!loops)
        diag(e) <- 0
    edges(e)
}

#' @title Density of edges
#' @description Compute the probability density for an \code{\link{edges}} object
#' @param x an R object for dispatch
#' @param edges an \code{\link{edges}} object
#' @param edgemod an \code{\link{edgemod}} object
#' @param na.rm remove NAs when calculating?
#' @param ... additional arguments
#' @return matrix same size as \code{edges$E} with density of each edge
#' @seealso \code{\link{dedges.sbm}} \code{\link{dedges.sbm}}
#' @export
dedges <- function(x, edges, edgemod, na.rm=TRUE, ...)
    UseMethod("dedges", x)

#' @title Density of edges
#' @description Compute the probability density for an \code{\link{edges}} object under an \code{\link{sbm}} object
#' @param edges an \code{\link{edges}} object
#' @param x an \code{\link{sbm}} object
#' @param edgemod an \code{\link{edgemod}} object
#' @param na.rm remove NAs when calculating?
#' @param ... additional arguments for \code{dedges.params}
#' @return matrix same size as \code{edges$E} with density of each edge
#' @export
#' @examples
#' ## make an sbm model, sample data then plot and print:
#' model <- sbmmod(dma(2,5), param_beta(1,1,1,1), edges_bern())
#' s <- model$r(100)
#' e <- redges(s, model$edge)
#' dedges(s, e, model$edge)
dedges.sbm <- function(x, edges, edgemod, na.rm=TRUE, ...){
    pmat <- parammat(x)
    dedges.numeric(pmat, edges, edgemod, na.rm=na.rm, ...)
}

#' likelihood of edges
#' @param x a matrix of parameters (with same size as \code{edges$E})
#' @param edges an \code{\link{edges}} object
#' @param edgemod an \code{\link{edgemod}} object
#' @param na.rm remove NAs when calculating?
#' @param ... additional arguments passed to \code{edgemod$logd}
#' @return likelihood of edges under the \code{\link{edgemod}} using parameters in matrix \code{pmat}
dedges.numeric <- function(x, edges, edgemod, na.rm=na.rm, ...){
    l <- edgemod$logd(edges$E, x, ...)
    if(!edges$loops)
        diag(l) <- 0.0
    s <- sum(l, na.rm=na.rm)
    if(edges$sym)
        s <- s/2
    s
}
