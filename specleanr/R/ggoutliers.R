#' @param x . the datacleaner object
#'
#' @param select . the species index or name for multiple species
#'
#' @param color \code{string}. Color of the bars. Default is \code{grey}.
#' @param select \code{vect}. Enter selected groups to be displayed especially if they are greater than 10.
#'      For example if the species are more than 10, the plot will be done in batches.
#' @param desc \code{logical} To either arrange the bars in ascending or descending order.
#'
#' @param ncol,nrow \code{integer} If number of groups are greater than 1, then number of rows and columns can be set.
#'      Check ggplot2 facet parameters on how the columns are set.

#'
#' @title Visualize the outliers identified by each method
#'
#' @importFrom stats aggregate
#'
#' @return ggplot object indicating outlier detection methods and number of outlier flagged.
#'
#' @export
#'
#'
ggoutliers <-  function(x,
                        select = NULL,
                        color='purple',
                        desc = TRUE,
                        ncol = 2,
                        nrow = 2){##sci mode--italicise

  outdf <- extractoutliers(x)

  if(x@mode==FALSE){

    if(all(outdf$totaloutliers<1)==TRUE) stop("Nothing to plot. No outliers were flagged by all methods")

    outdf$method <- factor(outdf$method , levels = outdf$method [order(outdf$totaloutliers , decreasing = desc)])

    meanval <- mean(outdf$totaloutliers)

  }else{
      if(length(unique(outdf$groups))>=10 && is.null(select)){

        stop("Provide a vectors of particular groups to plot, use the select parameter to provide a vector of groups.")

        }
     if(!is.null(select)) outdf <- outdf[outdf$groups %in% select, ] else outdf

      meanval <- aggregate(totaloutliers~groups, data = outdf, mean)

      colnames(meanval) <- c('groups', 'meanvalue')

      totfacets <-  ncol*nrow

      ngroups <- length(unique(outdf$groups))

      if(totfacets<ngroups)stop("The total number of ncol and nrow are not enough to enable facets. Adjust ncol and nrow to a equal a product of ", ngroups, ".")

    }

  if(nrow(outdf)>=7) {
    angle <- 45; hjust <- 1
  } else{
    angle <- 0; hjust <- 0.5
  }

  method = NULL; groups = NULL ; totaloutliers = NULL

  check_packages(pkgs = c("ggplot2", "tidytext"))

  meanvalue <- NULL

  pltout <- ggplot2::ggplot(data = outdf, ggplot2::aes(x= if(x@mode==TRUE) tidytext::reorder_within(method, -totaloutliers, groups) else x = method,
                                                y = totaloutliers))+

    ggplot2::geom_bar(stat = 'identity', fill=color)+

    ggplot2::theme_bw()+

    {if(x@mode==TRUE) ggplot2::facet_wrap(~groups, scales = 'free', ncol = ncol, nrow = nrow)}+

    {if(x@mode==TRUE) tidytext::scale_x_reordered()} +

    ggplot2::theme(legend.position = 'none',

                   panel.grid.major = ggplot2::element_blank(),

                   panel.grid.minor = ggplot2::element_blank(),

                   axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust),


                   axis.text = ggplot2::element_text(size = 10))+

    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

    {if(x@mode==FALSE){
      ggplot2::geom_hline(yintercept = meanval, linetype='twodash', linewidth = 1)

    }else{
      ggplot2::geom_hline(data = meanval, ggplot2::aes(yintercept = meanvalue),linetype='twodash', linewidth = 1)
    }}+

    ggplot2::labs(x="Outlier detection methods", y ='Number of outliers')

  return(pltout)
}

