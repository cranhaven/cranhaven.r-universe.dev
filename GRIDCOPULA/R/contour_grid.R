#' @title Draws the density / distribution function of a grid copula with contours and colors
#' @return Returns a graph of the density / distribution.
#' @param gc a grid type copula object.
#' @param FUN the name of the function to be applied (d.grid for density, p.grid for distribution), default is 'd.grid'.
#' @param color.name indicates the palette of colors.
#' @param color.size indicates the number of colors.
#' @param show.points a logical value indicating if the data must be showed or
#' not, default is FALSE. 
#' @param copula.domain a logical value indicating if the copula domain is used, default is TRUE.
#' @param normal.marginal a logical value indicating if the normal distribution is used as marginal distribution
#' for both variables x1 and x2. The default value is TRUE, otherwise the gaussian kernel is used as marginal distribution.
#' This argument is neccesary only if the argument copula.domain is FALSE.
#' @examples
#' n <- 500
#' x <- rgamma(n,4,1/2)
#' e <- rnorm(n,0,.3)
#' y <- sin(x+e)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' u <- Fx(x)
#' v <- Fy(y)
#' df <- cbind(u,v)
#' k <- 10
#' m <- 10
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' contour.grid(gc = copula.grid, FUN = 'd.grid', color.name = "rainbow")
#' contour.grid(gc = copula.grid, FUN = 'p.grid', color.name = "rainbow")
#' @export



contour_grid <- function(gc, FUN='d.grid', color.name="none",
                            color.size=7, show.points=FALSE,
                            copula.domain=TRUE, normal.marginal=TRUE) {
  mg<-gc
  if(copula.domain) {
    u1 <- seq(0, 1, length.out=101)
    u2 <- seq(0, 1, length.out=101)
  } else {
    u1 <- seq(1e-2,1-1e-2,length.out=101)
    u2 <- seq(1e-2,1-1e-2,length.out=101)
    if(is.null(mg$X)) {
      x1.media <- 0
      x2.media <- 0
      x1.desv <- 1
      x2.desv <- 1
      x1 <- qnorm(p=u1, mean=x1.media, sd=x1.desv)
      x2 <- qnorm(p=u2, mean=x2.media, sd=x2.desv)
      mg$X[,1] <- qnorm(p=mg$U[,1], mean=x1.media, sd=x1.desv)
      mg$X[,2] <- qnorm(p=mg$U[,2], mean=x2.media, sd=x2.desv)
      normal.marginal <- TRUE
    } else {
      if(normal.marginal) {
        x1.media <- mean(mg$X[,1])
        x2.media <- mean(mg$X[,2])
        x1.desv <- sd(mg$X[,1])
        x2.desv <- sd(mg$X[,2])
        x1 <- qnorm(p=u1, mean=x1.media, sd=x1.desv)
        x2 <- qnorm(p=u2, mean=x2.media, sd=x2.desv)
        f.indep <- outer(X=x1, Y=x2, 'normal.multiplication',
                         parameters=c(x1.media, x2.media, x1.desv, x2.desv))
      } else {
        x1.bw <- density(mg$X[,1])$bw
        x2.bw <- density(mg$X[,2])$bw
        x1 <- qkden(p=u1, kerncentres=mg$X[,1], bw=x1.bw)
        x2 <- qkden(p=u2, kerncentres=mg$X[,2], bw=x2.bw)
        f.indep <- outer(X=x1, Y=x2, kernel.multiplication,
                         parameters=list(mg$X[,1], mg$X[,2], x1.bw, x2.bw))
      }
    }
  }
  if(FUN=="d.grid") {
    f.u <- outer(u1, u2, 'd.grid', mg)
  } else {
    f.u <- outer(u1, u2, 'p.grid', mg)
  }
  if(copula.domain) {
    w1 <- u1
    w2 <- u2
    f.w <- f.u
    w1.lab <- "u"
    w2.lab <- "v"
    w1.min <- 0
    w1.max <- 1
    w2.min <- 0
    w2.max <- 1
    w1.lim <- c(0,1)
    w2.lim <- c(0,1)
    w1.marcas <- seq(0,1,by=0.25)
    w2.marcas <- seq(0,1,by=0.25)
    w1.puntos <- mg$U[,1]
    w2.puntos <- mg$U[,2]
  } else {
    w1 <- x1
    w2 <- x2
    if(FUN=="d.grid") {
      f.w <- f.indep * f.u
    } else {
      f.w <- f.u
    }
    w1.lab <- "x"
    w2.lab <- "y"
    w1.min <- signif(min(x1),3)
    w1.max <- signif(max(x1),3)
    w2.min <- signif(min(x2),3)
    w2.max <- signif(max(x2),3)
    w1.lim <- c(w1.min, w1.max)
    w2.lim <- c(w2.min, w2.max)
    w1.marcas <- signif(seq(w1.min, w1.max, length.out=5),3)
    w2.marcas <- signif(seq(w2.min, w2.max, length.out=5),3)
    w1.puntos <- mg$X[,1]
    w2.puntos <- mg$X[,2]
  }

  if(color.name=="none") {
#    contour(w1, w2, f.w, xlab=w1.lab, ylab="", xaxp=c(w1.min, w2.max, 4),
#            yaxp=c(w2.min, w2.max, 4), xlim=w1.lim, ylim=w2.lim)
    contour(w1, w2, f.w, xlab=w1.lab, ylab="", xlim=w1.lim, ylim=w2.lim)
    mtext(text=w2.lab, side=2, line=3, las=1)
    if(show.points) {
      points(w1.puntos, w2.puntos, pch=20, col="red")
    }
  } else {
    f.levels <- seq(0, 1.02*max(f.w), length.out=(color.size+1))
    puntos.color <- "black"
    if(color.name=="heat.colors") {
      paleta.color <- heat.colors(n=color.size)
    } else if(color.name=="rainbow") {
      paleta.color <- rainbow(n=color.size)
    } else if(color.name=="terrain.colors") {
      paleta.color <- terrain.colors(n=color.size)
    } else if(color.name=="topo.colors") {
      paleta.color <- topo.colors(n=color.size)
    } else if(color.name=="cm.colors") {
      paleta.color <- cm.colors(n=color.size)
    } else if(color.name=="gray") {
      paleta.color <- gray(seq(0,1,length.out=(color.size+1)))
      puntos.color <- "yellow"
    }
    if(show.points) {
      filled.contour(w1, w2, f.w, col=paleta.color, levels=f.levels,
                 xlab="u", ylab="", plot.axes={axis(1, w1.marcas);
                   axis(2, w2.marcas); points(w1.puntos, w2.puntos,
                                              pch=20, col=puntos.color)})
    } else {
      filled.contour(w1, w2, f.w, col=paleta.color, levels=f.levels,
                     xlab="u", ylab="", plot.axes={axis(1, w1.marcas);
                       axis(2, w2.marcas)})
    }
    mtext(text=w2.lab, side=2, line=3, las=1)
  }
  return(0)
}

