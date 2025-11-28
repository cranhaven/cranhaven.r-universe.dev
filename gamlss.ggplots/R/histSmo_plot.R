################################################################################
################################################################################
################################################################################
################################################################################
# density estimates for hisSmo() 
################################################################################
################################################################################
################################################################################
################################################################################
histSmo_plot <- function(x, 
             col_fill_bar = gray(.5), 
                  col_bar = "pink",
                 col_line = "darkblue",
               width_line = 1,
                 title, xlabel)
{
if (!is(x,"histSmo")) stop("this function is designted for histSmo objects" )
  discrete <- x$discrete
if (discrete) {
    #switch(type, hist = {
      freq <- x$counts/(sum(x$counts))
      xvar <- x$x
      dens <- x$density
      xname <- x$hist[["xname"]]
      #sum(density*(breaks[10]-breaks[9]))
      txt.title <- if (missing(title))  
        paste("smooth histogram from ",deparse(substitute(x)) )
      else title
      txt.xlabel <- if (missing(xlabel)) ""  
      else xlabel
        da <- data.frame(freq=freq, xvar=xvar, dens=dens)
  gg <- ggplot2::ggplot(data=da, ggplot2::aes(x=xvar, y=freq))+
        ggplot2::geom_col( col= col_bar, fill=col_fill_bar)+
        ggplot2::geom_line(ggplot2::aes(x=xvar, y=dens), col=col_line, lwd=width_line)+
        ggplot2::ylab("Frequency")+ xlab(txt.xlabel)+ggtitle(txt.title)
  } else 
  {
       counts <- x$hist[["counts"]]
         freq <- counts/sum(counts)
      density <- x$hist[["density"]]
         mids <- x$hist[["mids"]]
        xname <- x$hist[["xname"]]
    txt.title <- if (missing(title))  
    paste("smooth histogram of",xname)
    else title
          da <- data.frame( freq=density, mids=mids, x=x$x, density=x$density )
          gg <- ggplot2::ggplot(data=da, ggplot2::aes(x=mids, y=freq))+
                ggplot2::geom_col( col= col_bar, fill=col_fill_bar)+
                ggplot2::geom_line(ggplot2::aes(x=x, y=density), col=col_line, 
                                   lwd=width_line)+
                ggplot2::ylab("density")+ 
                ggplot2::xlab(xname)+
                ggplot2::ggtitle(txt.title)
  }
gg  
}
