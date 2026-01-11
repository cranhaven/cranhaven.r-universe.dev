#' @title Blocks object
#' @description create a blocks object
#' @details stores the block allocations and total number of blocks for a stochastic block model
#' @param z vector of block labels for each node
#' @param kappa maximum number of blocks
#' @return a \code{blocks} object
#' @examples
#' ## Assign six nodes to four blocks:
#' b <- blocks(c(1,1,2,3,4,4), 4)
#' print(b)
#' plot(b) ## shows id two nodes are members of the same block
#' @export
blocks <- function(z, kappa){
    z <- as.factor(z)
    if(missing(kappa))
        kappa <- nlevels(z)
    levels(z) <- 1:kappa

    structure(
        list(
            z = z,
            kappa = kappa,
            sizes = table(z),
            numnodes = length(z)
        )
       ,
        class = "blocks"
    )
}

#' @importFrom graphics image plot
#' @importFrom grDevices rainbow
#' @title Plot blocks
#' @description plots a block object
#' @details plot the block assignments in a \code{blocks} object as a matrix, color-coded by block membership
#' @param x a blocks object to plot
#' @param col colours for the plot
#' @param xaxt override \code{image} parameters
#' @param yaxt override \code{image} parameters
#' @param xlab override \code{image} parameters
#' @param ylab override \code{image} parameters
#' @param ... additional parameters for \code{image}
#' @return NULL
#' @examples
#' ## Assign six nodes to four blocks:
#' b <- blocks(c(1,1,2,3,4,4), 4)
#' plot(b)
#' ## note that the lower left corner has one 2x2 red square
#' ## indicating node 1 and 2 belong to the same block
#' @export
plot.blocks <- function(x, col, xaxt='n', yaxt='n', xlab="Nodes", ylab="Nodes", ...){
    tmp <- blockmat(x)
    if(missing(col)){
        if(x$kappa > 1){
            col <- c(0, grDevices::rainbow(x$kappa))
        } else{
            col <- grDevices::rainbow(x$kappa)
        }
    }
    n <- 1:x$numnodes-0.5
    graphics::image(n, n, t(tmp) %*% ((1:x$kappa) * tmp), col=col,
          xaxt=xaxt, yaxt=yaxt, xlab=xlab, ylab=ylab, ...)
}

#' @rdname plot.blocks
#' @export
image.blocks <- plot.blocks

#' @export
print.blocks <- function(x, ...)
    cat("blocks object\nkappa: ", x$kappa,
        "\nNumber of nodes: ", x$numnodes,
        "\nblock sizes: ", x$sizes,
        "\n")
