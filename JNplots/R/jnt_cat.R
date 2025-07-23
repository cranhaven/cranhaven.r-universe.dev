#' Calculation and visualization of regions of non-significance to assess the
#' influence of categorical moderators
#'
#' Produces a plot showing regions of non-significance defined by predictor values
#' for which no significant differences in the dependent variable are found
#' between categories.
#' @param X A character string defining the name of the covariate (e.g., size in
#'  an allometry analysis). Must be the same as the name of the variable in the
#'  dataset (see argument “data”).
#' @param Y A character string defining the name of the dependent variable. Must
#'  be the same as the name of the variable in the dataset (see argument “data”).
#' @param m A character string defining the name of a categorical moderator
#'  (e.g., males and females, herbivorous and carnivorous, etc). Must be the same
#'  as the name of the variable in the dataset (see argument “data”). The
#'  variable must have two levels.
#' @param data A dataframe containing the variables in the model.
#' @param alpha.sig A value representing the significance value (alpha) to be considered.
#' @param plot.full A logical. It indicates whether the plot should show the
#'  JN non-significance regions even if they don’t or just partially overlap the data.
#'  The default option is 'FALSE', meaning that the plot limits will depend only on the range of predictor values
#' @param correlation an optional \link{corStruct} object describing the within-group
#'  correlation structure. See the documentation of \link{corClasses} for a description of
#'  the available corStruct classes. If a grouping variable is to be used, it must be
#'  specified in the form argument to the corStruct constructor. Defaults to NULL,
#'  corresponding to uncorrelated errors.
#' @param cols A vector of strings defining the colors of the symbols to be used in the
#'  plot. By default, c('black', 'black') is used, which combines with the default
#'  in the argument 'pch' to present two groups of datapoints as open and close.
#' @param pch A vector of strings defining the symbols to be used to represent
#'  distinct groups in the plot. Use same symbol codes as in the argument 'pch'
#'  in the R base function 'plot'. By default, c(16, 1) is used, which combines
#'  with the default in the argument 'cols' to present two groups of datapoints
#'  as open and close.
#' @param cex number indicating the amount by which plotting symbols should
#' be scaled relative to the default (1).
#' @param xlab A title for the X axis. Defaults to the name of the predictor variable
#'  in the data.
#' @param ylab A title for the Y axis. Defaults to the name of the dependent variable
#'  in the data.
#' @param lty A vector defining the line type of the regression lines for each category.
#' Defaults to c(1,2).
#' @param line.col A vector of strings defining the line colors of the regression lines for each category.
#' Defaults to c('black','black').
#' @param lwd A vector defining the line width of the regression lines for each category.
#' Defaults to c(1,1).
#' @param legend A logical indicating whether a legend should appear on top of the plot. Defaults to 'TRUE'.
#' @return List with four elements: (1) results from the linear model, (2) lower and
#' (3) upper limits of non-significance in the predictor, and (4) a graphical output.
#' @import nlme scales ape
#' @importFrom grDevices colorRampPalette rgb recordPlot
#' @importFrom graphics abline par points polygon
#' @importFrom stats complete.cases na.omit qf vcov qt
#' @examples
#' data(microlophus)
#' jnt_cat(X='svl', Y='hl', m='species', data=microlophus,
#' xlab='log(SVL)', ylab='log(head length)')
#' @references Toyama, K. S. (2023). JNplots: an R package to visualize outputs
#' from the Johnson-Neyman technique for categorical and continuous moderators,
#' including options for phylogenetic regressions. bioRxiv, 2023-05.
#' @export

jnt_cat <- function(X,Y,m,data,alpha.sig=0.05,plot.full=FALSE,correlation=NULL,cols=c("black","black"),pch=c(16,1),
                    cex=1,xlab=X, ylab=Y, lty=c(1,2), line.col=c("black","black"),
                    lwd=c(1,1),legend=TRUE){
  na_sum <- (sum(is.na(data[,X])))+(sum(is.na(data[,Y])))+(sum(is.na(data$m)))
  m1 <- c("Rows with missing data were removed from the analysis")
  if(na_sum>0){
    warning(m1)
  }
  data <- data[complete.cases(data[ , c(X,Y,m)]), ]
  data[,m] <- as.factor(data[,m])
  levs <- levels(as.factor(droplevels(data[,m])))
  Xi <- data[,X]
  Yi <- data[,Y]
  gi <- data[,m]
  mod <- nlme::gls(Yi~Xi*gi, correlation=correlation,
             na.action = na.omit)
  mod.out <- summary(mod)

  group1 <- data[ which(data[,m]==levs[1]), ]
  group2 <- data[ which(data[,m]==levs[2]), ]

  n1 <- length(group1[,1])
  n2 <- length(group2[,1])
  X1 <- group1[,X]
  X2 <- group2[,X]
  Y1 <- group1[,Y]
  Y2 <- group2[,Y]

  Fcvalue <- qf(1-alpha.sig, df1=1, df2=n1+n2-4)
  xmean1 <- mean(X1)
  xmean2 <- mean(X2)
  xmeansq1 <- mean((X1)^2)
  xmeansq2 <- mean((X2)^2)
  sumx1 <- 0
  sumx2 <- 0
  sumy1 <- 0
  sumy2 <- 0
  sumxy1 <- 0
  sumxy2 <- 0

  xcoord_group1 <- X1    # X1
  ycoord_group1 <- Y1    # Y1

  xcoord_group2 <- X2    # X2
  ycoord_group2 <- Y2    # Y2

  zx1 <- ((sum(xcoord_group1,na.rm = TRUE))^2)/n1
  zx2 <- ((sum(xcoord_group2,na.rm = TRUE))^2)/n2
  zy1 <- ((sum(ycoord_group1,na.rm = TRUE))^2)/n1
  zy2 <- ((sum(ycoord_group2,na.rm = TRUE))^2)/n2
  zxy1 <- ((sum(xcoord_group1,na.rm = TRUE))*(sum(ycoord_group1,na.rm = TRUE)))/n1
  zxy2 <- ((sum(xcoord_group2,na.rm = TRUE))*(sum(ycoord_group2,na.rm = TRUE)))/n2

  ######## sumx1 ########
  c <- 0
  while (c<n1) {
    sumx1 <- sumx1 + (((xcoord_group1[c+1])^2) - zx1)
    c <- c+1
  }
  sumx1

  ######## sumx2 ########
  c <- 0
  while (c<n2) {
    sumx2 <- sumx2 + (((xcoord_group2[c+1])^2) - zx2)
    c <- c+1
  }
  sumx2

  ######## sumy1 ########
  c <- 0
  while (c<n1) {
    sumy1 <- sumy1 + (((ycoord_group1[c+1])^2) - zy1)
    c <- c+1
  }
  sumy1

  ######## sumy2 ########
  c <- 0
  while (c<n2) {
    sumy2 <- sumy2 + (((ycoord_group2[c+1])^2) - zy2)
    c <- c+1
  }
  sumy2

  ######## sumxy1 ########
  c <- 0
  while (c<n1) {
    sumxy1 <- sumxy1 + (((xcoord_group1[c+1])*(ycoord_group1[c+1]))-zxy1)
    c <- c+1
  }
  sumxy1

  ######## sumxy2 ########
  c <- 0
  while (c<n2) {
    sumxy2 <- sumxy2 + (((xcoord_group2[c+1])*(ycoord_group2[c+1]))-zxy2)
    c <- c+1
  }
  sumxy2

  ########################
  ########################
  ########################
  ######## SSres ########

  SSres <- (sumy1-(((sumxy1)^2)/sumx1))+(sumy2-(((sumxy2)^2)/sumx2))
  SSres

  ########################
  ########################
  ########################
  ######## ABC ########
  a1 <- mod$coefficients[1]
  a2 <- mod$coefficients[1]+mod$coefficients[3]
  b1 <- mod$coefficients[2]
  b2 <- mod$coefficients[2]+mod$coefficients[4]
  A <- (-Fcvalue/(n1+n2-4))*(SSres)*((1/sumx1)+(1/sumx2))+((b1-b2)^2)
  B <- (Fcvalue/(n1+n2-4))*(SSres)*((xmean1/sumx1)+(xmean2/sumx2))+((a1-a2)*(b1-b2))
  C <- (-Fcvalue/(n1+n2-4))*(SSres)*(((n1+n2)/(n1*n2))+(xmeansq1/sumx1)+(xmeansq2/sumx2))+((a1-a2)^2)

  ########################
  xlower <- (-B-sqrt((B^2)-A*C))/A
  xupper <- (-B+sqrt((B^2)-A*C))/A

  m2 <- c("It was not possible to calculate regions of non-significance. The difference between slopes might not be statistically significant")
  if(((B^2)-A*C)<0){
    warning(m2)
  }

  #### PLOT
  if(plot.full==TRUE){
    min.lim <- min(data[,X],xlower)
    max.lim <- max(data[,X],xupper)
    plot(data[,X],data[,Y],xlab=xlab,ylab=ylab,xlim=c(min.lim,max.lim),type="n")
    points(group1[,X],group1[,Y],col=cols[1],pch=pch[1])
    points(group2[,X],group2[,Y],col=cols[2],pch=pch[2])
    abline(a1,b1,lty=lty[1],col=line.col[1],lwd=lwd[1])
    abline(a2,b2,lty=lty[2],col=line.col[2],lwd=lwd[2])
    polygon(c(xlower,xlower,xupper,xupper),c(-2*(abs(min(data[,Y]))),max(data[,Y])*2,
                                             max(data[,Y])*2,-2*(abs(min(data[,Y])))),col=rgb(224, 224, 224,
                                                                                        maxColorValue=255,alpha=130), border=NA)
  }else{
    plot(data[,X],data[,Y],xlab=xlab,ylab=ylab,type="n")
    points(group1[,X],group1[,Y],col=cols[1],pch=pch[1],cex=cex)
    points(group2[,X],group2[,Y],col=cols[2],pch=pch[2],cex=cex)
    abline(a1,b1,lty=lty[1],col=line.col[1],lwd=lwd[1])
    abline(a2,b2,lty=lty[2],col=line.col[2],lwd=lwd[2])
    polygon(c(xlower,xlower,xupper,xupper),c(-2*(abs(min(data[,Y]))),max(data[,Y])*2,
                                             max(data[,Y])*2,-2*(abs(min(data[,Y])))),col=rgb(224, 224, 224,
                                                                                              maxColorValue=255,alpha=130), border=NA)
  }
  if(legend==TRUE){
    legend(par('usr')[1],par('usr')[4]+((par('usr')[4]-par('usr')[3])/6), bty='n', xpd=NA,
           c(levs[1], levs[2]),
           pch=c(pch[1],pch[2]),cex=0.8,col=c(cols[1],cols[2]))
  }
  pl <- recordPlot()
  if(((B^2)-A*C)<0){
    results <- list("coeff" = mod.out,pl)
    return(results)
  }else{
    results <- list("coeff" = mod.out,"lower limit in X" = xlower, "upper limit in X" = xupper,pl)
    return(results)
  }
}

