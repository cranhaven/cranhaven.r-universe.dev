################################################################################
################################################################################
# plot a histogram and density function for any variable
################################################################################
################################################################################
y_hist <- function(y,
                  data,
          with.density = TRUE,
              hist.col = "black", 
             hist.fill = "white",
             dens.fill = "#FF6666",
              binwidth = (max(y)-min(y))/20,
                  from, to, title)
{
################################################################################
other_prep_data <- function (y) 
{
  obs <- seq_len(length(y))
  obs <- obs[!is.na(y)]
    y <- y[!is.na(y)]
  out <- data.frame(obs = obs, y = as.numeric(y))
return(out)
}  
################################################################################ 
         xlab <- deparse(substitute(y))
if (missing(y))  stop("the y is not declared")
if (with.density) # with the density
{
txt.title <- if (missing(title))   paste("Histogram and density plot of",xlab)
    else title
if (!missing(data)) y <- get(xlab, envir = as.environment(data))
           xlimitfrom <- if (missing(from))   min(y) else from
             xlimitto <- if (missing(to))     max(y) else to  
          d <- other_prep_data(y) 
        # f <- d[d$color == "outlier", c("obs", "y")]
#colnames(f) <- c("observation", "y")
         gg <- ggplot2::ggplot(d, ggplot2::aes(x=y))+
               ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)), 
                              binwidth = binwidth, 
                              colour=hist.col, fill=hist.fill)+
               ggplot2::xlim(xlimitfrom, xlimitto)+ 
               ggplot2::geom_density(alpha=0.2, fill=dens.fill)+
               ggplot2::xlab(xlab) + 
               ggplot2::ylab("density") + 
               ggplot2::ggtitle(txt.title) 
}  else # without the density
{
    txt.title <- if (missing(title))   paste("Histogram of",xlab)
    else title
if (!missing(data))  y <- get(xlab, envir = as.environment(data))
           xlimitfrom <- if (missing(from))   min(y) else from
             xlimitto <- if (missing(to))     max(y) else to   
    gg <- ggplot2::ggplot(data.frame(y=y), ggplot2::aes(x=y))+
          ggplot2::geom_histogram(aes(y=ggplot2::after_stat(density)),
                                  binwidth = binwidth, 
                                  colour=hist.col, fill=hist.fill)+
          ggplot2::xlab(xlab) + 
          ggplot2::xlim(xlimitfrom, xlimitto)+      
          ggplot2::ylab("histogram") + 
          ggplot2::ggtitle(txt.title) 
}  
  return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
y_dots <- function(y,
                   data,
                     value = 3,
                point.size = 2,
                 point.col = "gray",
                  quantile = c(.10, .50, .90),
                  line.col = c("black","red", "black"),
                 line.type = c("dotted", "solid",  "dotted"),
                 line.size = c(1,1,1),
                x.axis.col = "black",
          x.axis.line.type = "solid",
                      seed = 123,
                   from, to, title)
{
################################################################################
other_prep_data <- function (y, seed, value) 
  {
      obs <- seq_len(length(y))
      obs <- obs[!is.na(y)]
        y <- y[!is.na(y)]
       ly <- length(y)
        G <- quantile(y, probs=c(.25,.75))
       IQ <- (G[2]-G[1])/2 
 value_lo <- G[1] - value * IQ
 value_up <- G[2] + value * IQ
set.seed(seed)
     rand <- runif(ly)
      out <- data.frame(obs = obs, rand=rand, y = as.numeric(y))
out$color <- ifelse(((out$y >= value_up) | out$y <= value_lo), 
                         c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = 
                                c("normal", "outlier"))
   out$txt <- ifelse(out$color == "outlier", out$obs, NA) 
  return(out)
  }  
################################################################################   
        rand <- txt <- NULL 
        xlab <- deparse(substitute(y))
if (missing(y))  stop("the y is not declared")
   txt.title <- if (missing(title))   paste("Plot of",xlab)
                 else title
if (!missing(data)) y <- get(xlab, envir = as.environment(data))
  xlimitfrom <- if (missing(from))   min(y) else from
    xlimitto <- if (missing(to))     max(y) else to  
           d <- other_prep_data(y, seed=seed, value=value) 
   gg <- ggplot2::ggplot(d, ggplot2::aes(x = y, y = rand, label = txt, 
                                         ymin = 0, ymax = rand)) +
         ggplot2::geom_point(size=point.size, col=point.col)  +
         ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, 
                  family = "serif", fontface = "italic", 
                  colour = "darkred", na.rm = TRUE)+  
        ggplot2::geom_vline(xintercept = quantile(y, quantile[1]), 
                 col=line.col[1], linewidth=line.size[1], linetype=line.type[1])+
        ggplot2::geom_vline(xintercept = quantile(y, quantile[2]), 
                 col=line.col[2], linewidth=line.size[2], linetype=line.type[2])+
        ggplot2::geom_vline(xintercept = quantile(y, quantile[3]), 
                            col=line.col[3], size=line.size[3], 
                            linetype=line.type[3])+
        ggplot2::ggtitle(txt.title) +
        ggplot2::xlim(xlimitfrom, xlimitto)+
        ggplot2::xlab(xlab) + 
        ggplot2::theme(axis.title.y=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
             panel.grid.major.y = ggplot2::element_blank(),
             panel.grid.minor.y = ggplot2::element_blank(),
             axis.line.x = ggplot2::element_line(size = 0.5, 
                  linetype = x.axis.line.type, colour = x.axis.col),
                    panel.background = ggplot2::element_blank())
  return(gg)
}
##########################################################################
##########################################################################
##########################################################################
##########################################################################


