#' @title Communities subniches dynamic
#' @aliases plot_dym
#' @description  The function represents the species' subniches SR position within the environmental space E.
#' @param subnic an object of class \code{subniche}.
#' @param sig_thres value for minimum significance, default 0.05
#' @param sig a factor defining the significance species, default NULL.
#' @param main a main title for the plot, see \link[graphics]{title} for more details.
#' @param xlab label for x-axis, see \link[graphics]{title} for more details.
#' @param ylab label for y-axis, see \link[graphics]{title} for more details.
#' @param col.arrow arrow color, see \link[graphics]{arrows} for more details.
#' @param angle.arrow arrow angle head, see \link[graphics]{arrows} for more details.
#' @param lwd.arrow arrow width, see \link[graphics]{arrows} for more details.
#' @param length.arrow arrow head length, see \link[graphics]{arrows} for more details.
#' @param border.E color border of E polygon, see \link[graphics]{polygon} for more details.
#' @param col.E inside color of E polygon, see \link[graphics]{polygon} for more details.
#' @param lty.E line type for the E border, see \link[graphics]{polygon} for more details.
#' @param lwd.E line width for the E border, see \link[graphics]{polygon} for more details.
#' @param col.axis axis color, see \link[graphics]{par} for more details.
#' @param lty.axis axis line type, see \link[graphics]{par} for more details.
#' @param lwd.axis  axis width, see \link[graphics]{par} for more details.
#' @param col.SR.pt point color contour if pch=21:25.
#' @param col.SR.pos color of points representing the SR position, see \link[graphics]{points} for more details.
#' @param pch.SR.pos type of points representing the SR position, see \link[graphics]{points} for more details.
#' @param cex.SR.pos size of points representing the SR position, see \link[graphics]{points} for more details.
#' @param col.SR.lab color of the species labels, see see \link[graphics]{text} for more details.
#' @param cex.SR.lab size of the species labels defautls NA for no labels, see see \link[graphics]{text} for more details.
#' @param fac.SR.lab factor for moving the SR labels from its original coordinates for clarity, by defaults they are multiply 1.2
#' @param leg a logical option for legend to be plotted or not, default leg=T.
#' @param bty.leg the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n", see \link[graphics]{legend} for more details.
#' @param posi.leg setting legend positions with the following keywords "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center", see \link[graphics]{legend} for more details.
#' @param font.sp An integer which specifies which font to use for species label. 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic, see \link[graphics]{par} for more details.
#' @param ...	further arguments passed to or from other methods.
#' @rdname plot_dym
#' @export plot_dym
#' @details The convex hulls measured is E is the environmental space.
#' The arrows represent the species' subniche marginality from the origin G.
#' See \doi{10.7717/peerj.3364} for more details on the subniche concept.
#' @examples
#' library(subniche)
#' data(doubs)
#' dudi1 <- dudi.pca(doubs$env, scale = TRUE, scan = FALSE, nf = 3)
#' nic1 <- niche(dudi1, doubs$fish, scann = FALSE)
#' # number of sites
#' N <- dim(nic1$ls)[1]
#' #Create a factor which defines the subsets
#' fact <- factor(c(rep(1,N/2),rep(2,N/2)))
#' # nic1 will be use as reference and fact will be use to define the subniches environment
#' subnic1 <- subniche(nic1, fact)
#' sigg <- rtestrefor(subnic1,10)
#' sig = c(sigg$`1`$witomigtest$subni.pvalue[-28],sigg$`2`$witomigtest$subni.pvalue[-28])
#' plot_dym(subnic1, sig=sig, sig_thres= 0.1)
#' @importFrom graphics par layout arrows points legend polygon abline text
#' @importFrom wordcloud textplot
#' @importFrom SIBER siberConvexhull
plot_dym <- function(subnic, sig=NULL, sig_thres=0.05,
                     xlab=NULL, ylab=NULL, main=NA, col.axis="azure3", lty.axis=2, lwd.axis=2,
                     border.E="black",
                     col.E="#92c5de",
                     lty.E=1,
                     lwd.E=1,
                     pch.SR.pos=21,
                     cex.SR.pos=1,
                     col.SR.pos= "#ffa600",
                     col.SR.pt="black",
                     col.SR.lab="black",
                     cex.SR.lab= NA,
                     fac.SR.lab=1.2,
                     col.arrow="black",  angle.arrow=20, lwd.arrow=2,
                        length.arrow=0.1,font.sp=2, leg=T, posi.leg="topleft", bty.leg="n", ...){

  fac <- subnic$factor
  lev <- levels(fac)
  ar_sub <- subarea(subnic)
  eig <- round(subnic$eig/sum(subnic$eig)*100,2)[1:2]
  E <- ar_sub$E
  if(pch.SR.pos<21|pch.SR.pos>25){
    col.SR.pt <- col.SR.pos
    }
  if(is.null(xlab)){
    xlab=paste(paste("OMI1",eig[1], sep=" "),"%",sep="")}
  if(is.null(ylab)){
    ylab=paste(paste("OMI2",eig[2], sep=" "),"%",sep="")}
  plot(subnic$ls, main=main, xlab= xlab,
       ylab= ylab, type="n",...)
  polygon(E$x, E$y, border=border.E, col=col.E, lty=lty.E, lwd=lwd.E)
  if(is.null(sig)){
    spli <- subnic$sub[!is.na(subnic$sub[,1]),]
  }else{
    spli <- subnic$sub[which(round(sig,2)<=sig_thres),]
    spli <- spli[!is.na(spli[,1]),]
  }
  sp <- rownames(spli)
  levi <- levels(subnic$factor)
  M <- length(levi)
  abline(h=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
  abline(v=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
  arrows(rep(0,M),rep(0,M),spli[,1], spli[,2], angle=angle.arrow,
         col=col.arrow,lwd=lwd.arrow, length=length.arrow)
  points(spli[,1], spli[,2], col=col.SR.pt, bg=col.SR.pos, pch=pch.SR.pos, cex=cex.SR.pos)
  if(!is.na(cex.SR.lab)){
    text(spli[,1]*fac.SR.lab, spli[,2]*fac.SR.lab, sp, col=col.SR.lab, font=font.sp, cex=cex.SR.lab)
  }
  if(isTRUE(leg)){
    filli <- c(col.E, NA)
    borderi <- c(border.E, NA)
    col.leg <- c(NA, col.SR.pt)
    col.bg <- c(NA, col.SR.pos)
    pch.leg <- c(NA,pch.SR.pos)
    posi.cex <-c(NA,cex.SR.pos)
    tex.leg <- c("E","SR position")

    if(anyNA(col.SR.pos)){
      pch.leg[2] <- NA
      col.leg[2] <- NA
      col.SR.pt[2] <- NA
      tex.leg[2] <- NA
      }
    lty.leg <- c(0,0)
    lwd.leg <- c(0,0)
    if(lty.E>1){
      pch.leg[1] <- NA
      lty.leg[1] <- lty.E
      lwd.leg[1] <- lwd.E
      }
    legend(posi.leg, legend=tex.leg,fill =filli, border=borderi, pch=pch.leg, col=col.leg, pt.cex = posi.cex,
           pt.bg=col.bg,lty=lty.leg,pt.lwd=c(NA,1), lwd=lwd.leg, bty=bty.leg,...)
  }
}
