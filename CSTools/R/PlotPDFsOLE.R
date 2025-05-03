#'Plotting two probability density gaussian functions and the optimal linear 
#'estimation (OLE) as result of combining them.
#'
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#'
#'@description This function plots two probability density gaussian functions 
#'and the optimal linear estimation (OLE) as result of combining them.
#'
#'@param pdf_1 A numeric array with a dimension named 'statistic', containg 
#'  two parameters: mean' and 'standard deviation' of the first gaussian pdf 
#'  to combining.
#'@param pdf_2 A numeric array with a dimension named 'statistic', containg 
#'  two parameters: mean' and 'standard deviation' of the second gaussian pdf 
#'  to combining.
#'@param nsigma (optional) A numeric value for setting the limits of X axis. 
#'  (Default nsigma = 3). 
#'@param legendPos (optional) A character value for setting the position of the
#'  legend ("bottom", "top", "right" or "left")(Default 'bottom'). 
#'@param legendSize (optional) A numeric value for setting the size of the 
#'  legend text. (Default 1.0). 
#'@param plotfile (optional) A filename where the plot will be saved. 
#'  (Default: the plot is not saved).
#'@param width (optional) A numeric value indicating the plot width in 
#'  units ("in", "cm", or "mm"). (Default width = 30). 
#'@param height (optional) A numeric value indicating the plot height. 
#'  (Default height = 15). 
#'@param units (optional) A character value indicating the plot size 
#'  unit. (Default units = 'cm'). 
#'@param dpi (optional) A numeric value indicating the plot resolution.
#'  (Default dpi = 300).
#'  
#'@return PlotPDFsOLE() returns a ggplot object containing the plot.
#'
#'@examples
#'# Example 1
#'pdf_1 <- c(1.1,0.6)
#'attr(pdf_1, "name") <- "NAO1"
#'dim(pdf_1) <-  c(statistic = 2)
#'pdf_2 <- c(1,0.5)
#'attr(pdf_2, "name") <- "NAO2"
#'dim(pdf_2) <-  c(statistic = 2)
#'
#'PlotPDFsOLE(pdf_1, pdf_2)
#'@import ggplot2
#'@export
PlotPDFsOLE <- function(pdf_1, pdf_2, nsigma = 3, legendPos = 'bottom', 
                        legendSize = 1.0, plotfile = NULL, width = 30, 
                        height = 15, units = "cm", dpi = 300) {
  y <- type <- NULL

  if(!is.null(plotfile)){
    if (!is.numeric(dpi)) {
      stop("Parameter 'dpi' must be numeric.")
    }
    if (length(dpi) > 1) {
      warning("Parameter 'dpi' has length greater than 1 and ",
              "only the first element will be used.")
      dpi <- dpi[1]
    }
    if (!is.character(units)) {
      stop("Parameter 'units' must be character")
    }
    if (length(units) > 1) {
      warning("Parameter 'units' has length greater than 1 and ",
              "only the first element will be used.")
      units <- units[1]
    }
    if(!(units %in% c("in", "cm", "mm"))) {
      stop("Parameter 'units' must be equal to 'in', 'cm' or 'mm'.")
    }
    if (!is.numeric(height)) {
      stop("Parameter 'height' must be numeric.")
    }
    if (length(height) > 1) {
      warning("Parameter 'height' has length greater than 1 and ",
              "only the first element will be used.")
      height <- height[1]
    }
    if (!is.numeric(width)) {
      stop("Parameter 'width' must be numeric.")
    }
    if (length(width) > 1) {
      warning("Parameter 'width' has length greater than 1 and ",
              "only the first element will be used.")
      width <- width[1]
    }
    if (!is.character(plotfile)) {
      stop("Parameter 'plotfile' must be a character string ",
           "indicating the path and name of output png file.")
    }
  }
  if (!is.character(legendPos)) {
    stop("Parameter 'legendPos' must be character")
  }
  if(!(legendPos %in% c("bottom", "top", "right", "left"))) {
    stop("Parameter 'legendPos' must be equal to 'bottom', 'top', 'right' or 'left'.")
  }
  if (!is.numeric(legendSize)) {
    stop("Parameter 'legendSize' must be numeric.")
  }
  if (!is.numeric(nsigma)) {
    stop("Parameter 'nsigma' must be numeric.")
  }
  if (length(nsigma) > 1) {
    warning("Parameter 'nsigma' has length greater than 1 and ",
            "only the first element will be used.")
    nsigma <- nsigma[1]
  }
  if (!is.array(pdf_1)) {
    stop("Parameter 'pdf_1' must be an array.")
  }
  if (!is.array(pdf_2)) {
    stop("Parameter 'pdf_2' must be an array.")
  }
  if (!is.numeric(pdf_1)) {
    stop("Parameter 'pdf_1' must be a numeric array.")
  }
  if (!is.numeric(pdf_2)) {
    stop("Parameter 'pdf_2' must be a numeric array.")
  }
  if (is.null(names(dim(pdf_1))) || 
      is.null(names(dim(pdf_2)))) {
    stop("Parameters 'pdf_1' and 'pdf_2' ",
         "should have dimmension names.")
  }
  if(!('statistic' %in% names(dim(pdf_1)))) {
    stop("Parameter 'pdf_1' must have dimension 'statistic'.")
  }
  if(!('statistic' %in% names(dim(pdf_2)))) {
    stop("Parameter 'pdf_2' must have dimension 'statistic'.")
  }
  if (length(dim(pdf_1)) != 1) {
    stop("Parameter 'pdf_1' must have only dimension 'statistic'.")  
  }
  if (length(dim(pdf_2)) != 1) {
    stop("Parameter 'pdf_2' must have only dimension 'statistic'.")  
  }
  if ((dim(pdf_1)['statistic'] != 2) || (dim(pdf_2)['statistic'] != 2)) {
    stop("Length of dimension 'statistic'",
         "of parameter 'pdf_1' and 'pdf_2' must be equal to 2.")
  }
  if(!is.null(attr(pdf_1, "name"))){
    if(!is.character(attr(pdf_1, "name"))){
      stop("The 'name' attribute of parameter 'pdf_1' must be a character ",
           "indicating the name of the variable of parameter 'pdf_1'.")
    }
  }
  if(!is.null(attr(pdf_2, "name"))){
    if(!is.character(attr(pdf_2, "name"))){
      stop("The 'name' attribute of parameter 'pdf_2' must be a character ",
           "indicating the name of the variable of parameter 'pdf_2'.")
    }
  }
  if(is.null(attr(pdf_1, "name"))){
    name1 <- "variable 1"
  } else {
    name1 <- attr(pdf_1, "name")
  }
  if(is.null(attr(pdf_2, "name"))){
    name2 <- "Variable 2"
  } else {
    name2 <- attr(pdf_2, "name")
  }
  
  #-----------------------------------------------------------------------------
  # Set parameters of gaussian distributions (mean and sd)
  #-----------------------------------------------------------------------------
  mean1 <- pdf_1[1]
  sigma1 <- pdf_1[2]
  mean2 <- pdf_2[1]
  sigma2 <- pdf_2[2]
  pdfBest <- CombinedPDFs(pdf_1, pdf_2)
  meanBest <- pdfBest[1]
  sigmaBest <- pdfBest[2]
  
  
  #-----------------------------------------------------------------------------
  # Plot the gaussian distributions 
  #-----------------------------------------------------------------------------
  nameBest <- paste0(name1, " + ", name2)
  graphicTitle <- "OPTIMAL LINEAR ESTIMATION"
  xlimSup <- max(nsigma * sigmaBest + meanBest, nsigma * sigma1 + mean1, 
                 nsigma * sigma2 + mean2)
  xlimInf <- min(-nsigma * sigmaBest+meanBest, - nsigma * sigma1 + mean1, 
                 -nsigma * sigma2 + mean2)
  # deltax <- 0.02
  deltax <- (xlimSup - xlimInf) / 10000
  
  x <- seq(xlimInf, xlimSup, deltax)
  df1 <- data.frame(x = x, y = dnorm(x, mean = mean1, sd = sigma1),
                    type = name1)
  df2 <- data.frame(x = x, y = dnorm(x, mean = mean2, sd = sigma2),
                    type = name2)
  df3 <- data.frame(x = x, y = dnorm(x, mean = meanBest, sd = sigmaBest),
                    type = nameBest)
  df123 <- rbind(df1, df2, df3)
  label1 <- paste0(name1, ": N(mean=",round(mean1, 2), ", sd=", round(sigma1, 2), 
                   ")")
  label2 <- paste0(name2, ": N(mean=",round(mean2, 2), ", sd=", round(sigma2, 2), 
                   ")")
  labelBest <- paste0(nameBest, ": N(mean=",round(meanBest,2), ", sd=", 
                      round(sigmaBest, 2), ")")  
  cols <- c("#DC3912", "#13721A", "#1F5094")
  names(cols) <- c(name1, name2, nameBest)
  g <- ggplot(df123) + geom_line(aes(x, y, colour = type), size = rel(1.2))
  
  g <- g + scale_colour_manual(values = cols, 
                                limits = c(name1, name2, nameBest), 
                                labels = c(label1, label2, labelBest))
  
  g <- g + theme(plot.title=element_text(size=rel(1.1), colour="black", 
                                         face= "bold"), 
                 axis.text.x = element_text(size=rel(1.2)),
                 axis.text.y = element_text(size=rel(1.2)),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 legend.position = legendPos, 
                 legend.text = element_text(face = "bold", size=rel(legendSize)))
  
  g <- g + ggtitle(graphicTitle)
  g <- g + labs(y="probability", size=rel(1.9))
  g <- g + stat_function(fun = dnorm_limit, args = list(mean=mean1, sd=sigma1),
                         fill = cols[name1], alpha=0.2, geom="area")
  g <- g + stat_function(fun = dnorm_limit, args = list(mean=mean2, sd=sigma2),
                         fill = cols[name2], alpha=0.2, geom="area")
  g <- g + stat_function(fun = dnorm_limit, args = list(mean=meanBest, 
                                                        sd=sigmaBest),
                         fill = cols[nameBest], alpha=0.2, geom="area")
  
 
  #-----------------------------------------------------------------------------
  # Save to plotfile if needed, and return plot
  #-----------------------------------------------------------------------------
  if (!is.null(plotfile)) {
    ggsave(plotfile, g, width = width, height = height, units = units, dpi = dpi)
  }
  return(g)
}

# Auxiliar function to plot
CombinedPDFs <- function(pdf_1, pdf_2) {
  mean_1 <- pdf_1[1]
  sigma_1 <- pdf_1[2]
  mean_2 <- pdf_2[1]
  sigma_2 <- pdf_2[2]
  a_1 <- (sigma_2^2)/((sigma_1^2)+(sigma_2^2))
  a_2 <- (sigma_1^2)/((sigma_1^2)+(sigma_2^2))
  pdf_mean <- a_1*mean_1 + a_2*mean_2
  pdf_sigma <- sqrt((sigma_1^2)*(sigma_2^2)/((sigma_1^2)+(sigma_2^2)))
  data <- c(pdf_mean, pdf_sigma)
  dim(data) <-  c(statistic = 2)
  return(data)
}

dnorm_limit <- function(x,mean,sd){
  y <- dnorm(x,mean,sd)
  y[x<mean | x > mean+sd] <- NA
  return(y)
}  
