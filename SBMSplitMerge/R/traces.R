#' helper function for trace plots
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_raster theme scale_x_continuous scale_y_continuous element_blank .data
#' @param mat matrix to plot as an image using ggplot2
#' @return `ggplot2` plot objecy
plotpostpairs <- function(mat){
    df <- reshape2::melt(mat)
    a <- ggplot2::aes(.data$Var2, .data$Var1, fill=.data$value)
    p <- ggplot2::ggplot(df, a) + ggplot2::geom_raster() +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
                       panel.grid.major=ggplot2::element_blank())
    p
}

#' plot a trace of the blocks from MCMC samples
#' @importFrom ggplot2 xlab ylab scale_fill_discrete
#' @param postz output from sampler
#' @param burnin which iterations to plot? defaults to all.
#' @return `ggplot2` object
#' @export
blocktrace <- function(postz, burnin){
    df <- reshape2::melt(postz[,burnin])
    names(df) <- c("Node", "Iteration", "Block")
    df$Block <- factor(df$Block)
    df$Iteration <- df$Iteration + min(burnin)
    a <- ggplot2::aes(.data$Iteration, .data$Node, fill=.data$Block)
    p <- ggplot2::ggplot(df, a) + ggplot2::geom_raster() +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
                       panel.grid.major=ggplot2::element_blank())
    p
}

#' plot a trace of the number of blocks from MCMC samples
#' @importFrom ggplot2 ggplot aes geom_line
#' @param postk output from sampler
#' @param burnin which iterations to plot? defaults to all.
#' @return `ggplot2` object
#' @export
numblockstrace <- function(postk, burnin){
    ggplot2::ggplot(data=data.frame(Iteration=burnin, K=postk[burnin]), ggplot2::aes(x=.data$Iteration, y=.data$K)) + ggplot2::geom_line()
}

#' plot a trace of parameter values from MCMC samples
#' @importFrom reshape2 melt
#' @importFrom scales hue_pal
#' @importFrom ggplot2 ggplot geom_line scale_color_manual
#' @param theta output from sampler
#' @param range which thetas to plot? defaults to all.
#' @param burnin which iterations to plot? defaults to all.
#' @return `ggplot2` object
#' @export
paramtrace <- function(theta, range, burnin){
    if(missing(range))
        range <- 1:dim(theta)[2]
    if(missing(burnin))
        burnin <- 1:dim(theta)[3]
    dimtheta <- dim(theta)[1]
    thetas <- theta[,range,burnin,drop=FALSE]
    ps <- list()
    length(ps) <- dimtheta
    for(k in 1:dimtheta){
        df <- data.frame(burnin, cbind(t(thetas[k,,])))
        names(df) <- c("burnin", range-1)
        mf <- reshape2::melt(df, id="burnin")
        names(mf) <- c("Iteration", "Theta", "Value")
        ps[[k]] <- ggplot(mf, aes(x=.data$Iteration, y=.data$Value, col=.data$Theta)) +
            geom_line() +
            ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(length(range)-1)))
    }
    ps
}

#' mean proportion of times two nodes were in the same block under MCMC samples
#' @param postz output from sampler
#' @return matrix P with P[i,j] = proportion of times i and j are in the same block under \code{postz}
postpairs <- function(postz){
    N <- nrow(postz)
    P <- matrix(0,N,N)
    for(i in 2:N)
        for(j in 1:(i-1))
            P[j,i] <- P[i,j] <- mean(postz[i,] == postz[j,])
    P
}

#' modal block assignments from MCMC samples
#' @export
#' @param postz output from sampler
#' @return a blocks object with the modal block assignments under \code{postz}
modeblocks <- function(postz)
    blocks(apply(postz, 1, function(x) which.max(tabulate(x, max(postz)))))


#' get a set of evaluation plots from MCMC samples
#' @export
#' @param output from sampler
#' @param burnin burn-in period (a vector of iteration numbers to subset outputs)
#' @param theta_index which set of thetas to plot?
#' @return list of ggplot objects (with descriptive names)
eval_plots <- function(output, burnin, theta_index){
    if(missing(burnin))
        burnin <- 1:output$nsteps
    nb <- numblockstrace(output$postk, burnin)
    pp <- postpairs(output$postz[,burnin])
    pp_plot <- plotpostpairs(pp) + ggplot2::xlab("Node") + ggplot2::ylab("Node") + ggplot2::scale_fill_continuous(name = "Probability")
    ## order for posterior plotting
    ind <- order(colSums(pp))
    pp_sorted <- plotpostpairs(pp[ind,ind]) + ggplot2::xlab("Node") + ggplot2::ylab("Node") + ggplot2::scale_fill_continuous(name = "Probability")
    bt <- blocktrace(output$postz, burnin)
    bt_sorted <- blocktrace(output$postz[ind,], burnin)
    pt <- paramtrace(output$postt, theta_index, burnin)
    pt_sorted <- paramtrace(output$postt, theta_index, burnin)
    list(
        num_blocks_trace = nb
       ,
        post_pairs = pp_plot
       ,
        post_pairs_sorted = pp_sorted
       ,
        blocks_trace = bt
       ,
        param_trace = pt
       ,
        param_trace_sorted = pt_sorted
       ,
        blocks_trace_sorted = bt_sorted
       ,
        pp=pp
       ,
        sortind = ind
    )
}
