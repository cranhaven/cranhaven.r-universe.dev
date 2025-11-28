
#' Makes 3D plots of Index of Moderated Mediation of gemm object
#'
#' @param x results of gemm function
#' @param ... optional
#' @return empty, directly plots all indices of mediation
#' @export

plotIMM3d <- function(x, ...) {
  
  if (is.null(x$intermediate$xdichotomous) | is.null(x$intermediate$ydichotomous)) 
     {return(message("No plot is constructed, because there are no two moderators specified"))}
  
  if (x$intermediate$xdichotomous & x$intermediate$ydichotomous) 
     {return(message("No plots are constructed, because both moderators are dichotomous"))}
    
  data <- x$intermediate$data 
  xmmod <- x$input$xmmod
  mymod <- x$input$mymod
  mvars <- x$input$mvars
  
  
  if (is.null(xmmod)) { return(message(" moderator x-m path not specified")) }
  ifelse(is.factor(x$input$data[,xmmod]), modLevels <- levels(x$input$data[,xmmod]), modLevels <- c(0,1))
  
  if (x$intermediate$xdichotomous) {
    Modxm  <- c(0,1) 
  } else {
    Modxm <- stats::quantile(as.numeric(data[,xmmod]), c(.10,.20,.40,.60,.80,.90))
     }
   
  if (is.null(mymod)) { return(message(" moderator m-y path not specified")) }
  ifelse(is.factor(x$input$data[,mymod]), modLevels <- levels(x$input$data[,mymod]), modLevels <- c(0,1))
  
  if (x$intermediate$ydichotomous) { 
    Modmy  <- c(0,1) 
    } else {
      Modmy <- stats::quantile(as.numeric(data[,mymod]), c(.10,.20,.40,.60,.80,.90))
     } 
  
  parEst <- lavaan::parameterestimates(x$intermediate$result)
  mm <- expand.grid(x = Modxm, y = Modmy)
  
  for (i in seq_along(mvars)){
    
    df <- prepIMM3d(mm$x, mm$y, parEst = parEst, i=i )
    z <- matrix(df[,2], nrow = length(Modxm), ncol = length(Modmy))
    upzlim <- max(max(z),.4) + .1
    lwzlim <- min(min(z),-.4) - .1

    if (!x$intermediate$xdichotomous & !x$intermediate$ydichotomous) { 
      
    graphics::persp(x=Modxm, y = Modmy, z = z, zlab = "IMM", xlab = "Mod1", ylab ="Mod2", 
          main = paste0("Index of Moderated Mediation of ab-path for mediator: ", mvars[i]),
          theta = 30, phi = 30,axes = TRUE, scale = TRUE, zlim = c(lwzlim,upzlim),
          ticktype = "detailed",nticks = 4,
          cex.lab = .8, cex.axis = .5, col = "lightgrey", shade = 0.3, 
          expand = 1, d=2, r=5, border = NA)
    
    } else {
    df2 <- cbind(mm,df)
    if (x$intermediate$xdichotomous) { 
       df2$fac <- df2$x; 
       df2$x <- df2$y; 
       modlab <- xmmod 
       mod <- mymod
       }
    if (x$intermediate$ydichotomous) { 
       df2$fac <- df2$y; 
       modlab <- mymod 
       mod <- xmmod
    }
    
    p <- ggplot(df2, aes(x=x, y=df2$est, colour = as.factor(df2$fac))) + geom_line() +
         ylim(lwzlim,upzlim) +
         xlab(paste0("Numerical moderator: ", mod)) +
         ylab(paste0("IMM")) +
         scale_colour_discrete(name  = modlab, labels = modLevels) +
         ggtitle(paste0("Index of Moderated Mediation for mediator: ", mvars[i] )) 
    graphics::plot(p)
    }
    
  }
  invisible(z)
}


#' Computes Index of moderated mediation of gemm object
#'
#' @param M1 moderator of x-m path  
#' @param M2 moderator of m-y path
#' @param i index of vector of mediators names
#' @param parEst parameter estimates from lavaan results
#' @return vector of index of moderated mediation with CI limits for a given mediator
#' @export

prepIMM3d <- function(M1, M2, parEst=parEst, i=1) {
  
  ind <- subset(parEst, grepl("ind", parEst$label))[,c("ci.lower","est","ci.upper")]
  mmx <- subset(parEst, grepl("modmedx", parEst$label))[,c("ci.lower","est","ci.upper")]
  mmy <- subset(parEst, grepl("modmedm", parEst$label))[,c("ci.lower","est","ci.upper")]
  im <- subset(parEst, grepl("im", parEst$label))[,c("ci.lower","est","ci.upper")]
  iy <- subset(parEst, grepl("iy", parEst$label))[,c("ci.lower","est","ci.upper")]
  
  N <- length(M1)
  p1 <- rep(1,N) %o% (unlist(ind[i,])) 
  p2 <- M1 %o% as.numeric(mmx[i,])
  p3 <- M2 %o% as.numeric(mmy[i,])
  p4 <- (M2 * M1) %o% (as.numeric(im[i,]) *  as.numeric(iy[i,2]))
  index <- p1 + p2 + p3 + p4

  return(index)
}













