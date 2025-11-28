################################################################################
################################################################################
################################################################################
################################################################################
# TO DO
# probably I should have a option to save the ecdf
################################################################################
################################################################################
################################################################################
################################################################################
y_ecdf <- function(y,
                      type = c("Owen", "JW"), 
                conf.level = c("95", "99"),
                     value = 2, 
                points.col = "steelblue4", 
             check_overlap = TRUE,
             show.outliers = FALSE, 
             from, to, title,  ...)
{
################################################################################
# local functions
################################################################################  
################################################################################
## this is the NPL.bands() function of  Ross Darnell from package SMIR
################################################################################
NPOwen.bands <- function (x, conf.level = c("95", "99")) 
  {
  if (!is.numeric(x)) stop("argument must be numeric")
    conf.level <- match.arg(conf.level)
            yn <- table(x)
            yi <- as.numeric(names(yn))
            cn <- as.numeric(cumsum(yn))
            nn <- rep(sum(yn) + 1, length(cn))
             p <- as.numeric(cn/nn)
if (conf.level == "95") 
    {
      lambda <- ifelse(nn <= 100, (3.0123 + 0.4835 * log(nn) - 
                           0.00957 * (log(nn))^2 - 0.001488 * (log(nn))^3), 
                       (3.0806 + 0.4894 * log(nn) - 0.02086 * (log(nn))^2))
    }
    else 
    {
      if (conf.level == "99") 
      {
        lambda <- ifelse(nn <= 100, (-4.626 - 0.541 * log(nn) + 
                        0.0242 * (log(nn))^2), (-4.71 - 0.512 * log(nn) + 
                                          0.0219 * (log(nn))^2))
      }
    }
  lambda <- sqrt(2 * lambda)
     phi <- pbeta(p, 1/3, 1/3)
      se <- 1/(5.3 * sqrt(nn * (p * (1 - p))^(1/3)))
    phiu <- phi + lambda * se
    phiu <- ifelse(phiu > 1, 1, phiu)
    phil <- phi - lambda * se
    phil <- ifelse(phil < 0, 0, phil)
      pu <- qbeta(phiu, 1/3, 1/3)
      pl <- qbeta(phil, 1/3, 1/3)
  list(x = yi, lower = pl, upper = pu)
  }
################################################################################  
NPJagerWellner.bands <- function (x, conf.level = c("95", "99")) 
  {
if (!is.numeric(x)) 
      stop("argument must be numeric")
conf.level <- match.arg(conf.level)
        yn <- table(x)
        yi <- as.numeric(names(yn))
        cn <- as.numeric(cumsum(yn))
        nn <- rep(sum(yn) + 1, length(cn))
         p <- as.numeric(cn/nn)
if (conf.level == "95") 
    {
      lambda <- ifelse(nn <= 100, (3.6792 + 0.5720 * log(nn) - 
                    0.0567 * (log(nn))^2 + 0.0027 * (log(nn))^3), 
                       (3.7752 + 0.5062 * log(nn) - 0.0417 * (log(nn))^2)+
                         0.0016* (log(nn)^3))
    }
    else 
    {
      if (conf.level == "99") {
    lambda <- ifelse(nn <= 100, (5.3318 + 0.5539 * log(nn) - 
                                       0.00370 * (log(nn))^2), 
                         (5.6392 + 0.4018 * log(nn) - 
                            0.0183 * (log(nn))^2))
      }
    }
    lambda <- sqrt(2 * lambda)
       phi <- pbeta(p, 1/3, 1/3)
        se <- 1/(5.3 * sqrt(nn * (p * (1 - p))^(1/3)))
      phiu <- phi + lambda * se
      phiu <- ifelse(phiu > 1, 1, phiu)
      phil <- phi - lambda * se
      phil <- ifelse(phil < 0, 0, phil)
        pu <- qbeta(phiu, 1/3, 1/3)
        pl <- qbeta(phil, 1/3, 1/3)
    list(x = yi, lower = pl, upper = pu)
  }
################################################################################
other_prep_data <- function (y, value=2, na.rm=TRUE) 
  {
   # color <- NULL
       iqr <- IQR(y, na.rm=na.rm)
       med <- median(y)
    valuel <- med-iqr*value
    valueu <- med+iqr*value
   # y_out <- ifelse(y >=valueu|y <= valuel, TRUE, FALSE)
       obs <- seq_len(length(y))
    # outlier <- y[y_out]
       obs <- obs[!is.na(y)]
         y <- y[!is.na(y)]
      fcdf <- ECDF(y) # create a function 
        mm <- fcdf(y) # evaluate it 
       out <- data.frame(obs = obs, y = y, scores=mm)
 out$color <- ifelse(((out$y >= valueu) | (out$y <= valuel)), 
                        c("outlier"), c("normal"))
    out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                                                           "outlier"))
    out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
  }    
################################################################################
################################################################################   
# main starts here
if (missing(y))   stop("the first argument y is needed")
     xlimitfrom <- if (missing(from))   min(y) else from
       xlimitto <- if (missing(to))     max(y) else to  
  conf.level <- match.arg(conf.level)
        type <- match.arg(type)
           d <- other_prep_data(y, value=value) 
           x <- lower <- upper <- scores <- txt <- NULL
      xlabel <- deparse(substitute(y))  
   txt.title <- if (missing(title))  paste("ECDF of", deparse(substitute(y)))
                else title  
          dx <- as.data.frame(switch(type, "Owen" = NPOwen.bands(d$y, 
                      conf.level=conf.level),
                    "JW"=NPJagerWellner.bands(d$y, conf.level=conf.level)))
  gg <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(data=dx, ggplot2::aes(ymin = lower, ymax = upper, 
                            x = x), alpha = 0.2)+
        ggplot2::stat_ecdf(data=d, ggplot2::aes(x = y, y = scores), 
                          geom = "step", color=points.col)+
        ggplot2::xlab(xlabel) + 
        ggplot2::ylab(paste("ECDF and", paste0(conf.level,"%", sep=""),"C.I.")) +
        ggplot2::geom_hline(yintercept = 0, colour = "gray")+
        ggplot2::geom_hline(yintercept = 1, colour = "gray") +
        ggplot2::ggtitle(txt.title)+
        ggplot2::xlim(xlimitfrom, xlimitto)+ 
if  (show.outliers)  ggplot2::geom_text(data = d, ggplot2::aes(x = y,
                   y = scores, label = txt),hjust = -0.1, nudge_x = 0.05, 
                   size = 3,check_overlap = check_overlap, family = "serif", 
                    fontface = "italic", colour = "darkred", na.rm = TRUE)
  # suppressWarnings(print(gg))
  return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
