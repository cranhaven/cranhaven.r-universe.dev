###############################################################################
## plot.varrank.R ---
## Author          : Gilles Kratzer
## Last modified   : 13/12/2017
##                 : 06/02/2018 (S3method)
###############################################################################

plot.varrank <- function(x,
                         ## block sepration
                         colsep = TRUE,
                         rowsep = TRUE,
                         sepcol ="white",
                         sepwidth=c(0.005,0.005),

                         ## cell labeling
                         cellnote = TRUE,
                         notecex = 1.5,
                         notecol = "black",
                         digitcell = 3,

                         ## Row/Column Labeling
                         margins = c(6, 6, 4, 2),
                         labelscex = 1.2,

                         ## color key + density info
                         colkey = NULL,
                         densadj = 0.25,
                         textlines = 2,

                         ## plot labels
                         main = NULL,
                         maincex = 1,
                         ...
){

  ##scaling to [0,1]:
  scale201 <- function(x, low=min(x), high=max(x) ) return( (x-low)/(high - low))

  x.algo <- x$algorithm

  x.scheme <- x$scheme

  x <- x[[2]]

  if(length(dimx <- dim(x)) != 2 || !is.numeric(x))
    stop("varrank object 'x' must be a numeric matrix.")

  n <- dimx[1]
  n.2 <- dimx[2]

  if(n <= 1)
    stop("varrank object 'x' must have at least 2 rows and 2 columns.")

  if(!is.numeric(margins) || length(margins) != 4)
    stop("'margins' must be a numeric vector of length 4.")


  if(is.null(colkey)){
    ##color rainbow
    ##definition of mypalette
    cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
    warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
    cols = c(rev(cool), rev(warm))
    mypalette <- colorRampPalette(cols)(255)
  }else{
    mypalette <- colkey
  }

  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  ##layout
  if(x.algo=="forward"){
    layout(matrix(c(1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1), nrow = 10, ncol = 10, byrow = TRUE))

  }
  if(x.algo=="backward"){
    layout(matrix(c(1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2), nrow = 10, ncol = 10, byrow = TRUE))

  }

  par(mar = margins)

  extreme <- max(abs(x), na.rm=TRUE)
  breaks <- length(mypalette)
  breaks <- seq( -extreme, extreme, length=breaks + 1)


  image(1:n.2, 1:n, t(x[n:1,]), xlim = 0.5 + c(0, n), ylim = 0.5 + c(0, n),
        axes = FALSE, xlab = "", ylab = "", col=mypalette, breaks = breaks, ...)




  if(x.algo=="forward"){
    ## add 'background' colored spaces to visually separate sections
    if(colsep) {
      rect(xleft =.5,               ybottom=0,
           xright=.5+sepwidth[1],   ytop=nrow(x)+1.5,
           lty=1, lwd=1, col=sepcol, border=sepcol)

      for(csep in 1:min(n.2,n-1)){
        rect(xleft =csep+0.5,               ybottom=0,
             xright=csep+0.5+sepwidth[1],   ytop=nrow(x)+1.5-csep,
             lty=1, lwd=1, col=sepcol, border=sepcol)
      }
    }
    if(rowsep) {
      for(rsep in 1:n){
        rect(xleft =0.5,          ybottom= (nrow(x)+1-rsep)-0.5,
             xright=1.5+min(rsep,n.2-1,n-2),  ytop   = (nrow(x)+1-rsep)-0.5 - sepwidth[2],
             lty=1, lwd=1, col=sepcol, border=sepcol)
      }
      rect(xleft =0.5,          ybottom= (nrow(x)+1)-0.5,
           xright=1.5        ,  ytop   = (nrow(x)+1)-0.5 - sepwidth[2],
           lty=1, lwd=1, col=sepcol, border=sepcol)

    }
    axis(1,
         1:n,
         labels= rownames(x[1:n,]),
         las= 2,
         tick= 0,
         cex.axis= labelscex
    )
  }
  if(x.algo=="backward"){


    ###############
    n.2 <- n.2 - 1

    ## add 'background' colored spaces to visually separate sections
    if(colsep){
      rect(xleft =.5,               ybottom=0.5,
           xright=.5+sepwidth[1],   ytop=nrow(x)+.5,
           lty=1, lwd=1, col=sepcol, border=sepcol)
      for(csep in 1:min(n.2,n-1)){
        rect(xleft =csep+0.5,               ybottom=-0.5+csep,
             xright=csep+0.5+sepwidth[1],   ytop=nrow(x)+.5,
             lty=1, lwd=1, col=sepcol, border=sepcol)
      }
    }
    if(rowsep) {
      for(rsep in 1:n){
        rect(xleft =0.5,                    ybottom= (nrow(x)+1-rsep)-0.5,
             xright=1.5+min(n-rsep, n.2-1),  ytop   = (nrow(x)+1-rsep)-0.5 - sepwidth[2],
             lty=1, lwd=1, col=sepcol, border=sepcol)
      }
      rect(xleft =0.5,          ybottom= (nrow(x)+1)-0.5,
           xright=n.2+.5        ,  ytop   = (nrow(x)+1)-0.5 - sepwidth[2],
           lty=1, lwd=1, col=sepcol, border=sepcol)
    }
    axis(3,
         1:n,
         labels= rownames(x[n:1,]),
         las= 2,
         tick= 0,
         cex.axis= labelscex
    )
  }
  axis(2,
       1:n,
       labels= rownames(x[n:1,]),
       las= 2,
       tick= 0,
       cex.axis= labelscex
  )

  if(cellnote){
    cellnote <- round(x = x,digits = digitcell)[n:1,]

    text(x=c(col(cellnote)),
         y=c(row(cellnote)),
         labels=c(cellnote),
         col=notecol,
         cex=notecex)
  }

  ## title
  if(!is.null(main)) title(main, cex.main = 1.5*maincex)

  ##key
  zlim <- max(abs(min(breaks)),abs(max(breaks)))
  z <- seq(from = -zlim,to = zlim, length=length(mypalette))
  image(z=matrix(z, ncol=1),
        col=mypalette,breaks = breaks,
        xaxt="n", yaxt="n",ylim = c(0,1))

  ##density
  dens <- density(x, adjust=densadj, na.rm=TRUE,from=min(breaks), to=max(breaks))
  dens$x <- scale201(dens$x, min(breaks), max(breaks))
  lines(dens$x, dens$y / max(dens$y) * 0.95, col="black", lwd=1)


  yargs <- list(at=pretty(dens$y/max(dens$y)) * 0.95, labels=pretty(dens$y/max(dens$y)))
  yargs$side <- 2
  do.call(axis, yargs)

  title("Score density")

  lv <- pretty(breaks)
  xv <- scale201(as.numeric(lv), min(breaks), max(breaks))
  xargs <- list(at=xv, labels=lv)

  xargs$side <- 1
  do.call(axis, xargs)
  mtext(side=1, "Redundancy        Relevancy", line = textlines, cex = 0.8*maincex)
  invisible()
}#EOF
