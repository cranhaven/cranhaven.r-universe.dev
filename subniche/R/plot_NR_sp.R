#' @title Species niche
#' @aliases plot_NR_sp
#' @description The function represents the species' niche NR within the environmental space E.
#' @param subnic an object of class \code{subniche}.
#' @param sp a character string of the species name, default rownames(subnic$li)
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
#' @param border.NR color border of NR polygon, see \link[graphics]{polygon} for more details.
#' @param col.NR inside color of NR polygon, see \link[graphics]{polygon} for more details.
#' @param lty.NR line type for the NR border, see \link[graphics]{polygon} for more details.
#' @param lwd.NR line width for the NR border, see \link[graphics]{polygon} for more details.
#' @param col.NR.pt point color contour if pch=21:25.
#' @param col.NR.pos color of points representing the NR position, see \link[graphics]{points} for more details.
#' @param pch.NR.pos type of points representing the NR position, see \link[graphics]{points} for more details.
#' @param cex.NR.pos size of points representing the NR position, see \link[graphics]{points} for more details.
#' @param col.NR.lab color of the species labels, see see \link[graphics]{text} for more details.
#' @param cex.NR.lab size of the species labels defaults NA for no labels, see see \link[graphics]{text} for more details.
#' @param fac.NR.lab factor for moving the NR labels from its original coordinates for clarity, by defaults they are multiply 1.2
#' @param leg a logical option for legend to be plotted or not, default leg=T.
#' @param bty.leg the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n", see \link[graphics]{legend} for more details.
#' @param posi.leg setting legend positions with the following keywords "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center", see \link[graphics]{legend} for more details.
#' @param font.sp An integer which specifies which font to use for species label. 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic, see \link[graphics]{par} for more details.
#' @param ...	further arguments passed to or from other methods.
#' @rdname plot_NR_sp
#' @export plot_NR_sp
#' @details The convex hulls measured is E is the environmental space.
#' The arrows represent the species' NR marginality from the origin G.
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
#' plot_NR_sp(subnic1, "Neba")
#' @importFrom graphics par layout arrows points legend polygon abline text
#' @importFrom wordcloud textplot
#' @importFrom SIBER siberConvexhull
plot_NR_sp <- function(subnic, sp,
                    xlab=NULL, ylab=NULL, main=NA, col.axis="azure3", lty.axis=2, lwd.axis=2,
                    border.E="black",
                    col.E="#92c5de",
                    lty.E=1,
                    lwd.E=1,
                    border.NR ="black",
                    col.NR = "#fdb462",
                    lty.NR=1,
                    lwd.NR=1,
                    pch.NR.pos=21,
                    cex.NR.pos=1,
                    col.NR.pos= "#a1d99b",
                    col.NR.pt="black",
                    col.NR.lab="black",
                    cex.NR.lab= NA,
                    fac.NR.lab=1.2,
                    col.arrow="black",  angle.arrow=20, lwd.arrow=2,
                    length.arrow=0.1,font.sp=2, leg=T, posi.leg="topleft", bty.leg="n", ...){

  eig <- round(subnic$eig/sum(subnic$eig) * 100, 2)
  ar_sub <- subarea(subnic)
  E <- ar_sub$E
  NR <- ar_sub$NR[[sp]]
  li <- subnic$li[sp,]
  if(pch.NR.pos<21|pch.NR.pos>25){
    col.NR.pt <- col.NR.pos
  }
  if(is.null(xlab)){
    xlab=paste(paste("OMI1",eig[1], sep=" "),"%",sep="")}
  if(is.null(ylab)){
    ylab=paste(paste("OMI2",eig[2], sep=" "),"%",sep="")}
  plot(subnic$ls, main=main, xlab= xlab,
       ylab= ylab, type="n",...)
  polygon(E$x, E$y, border=border.E, col=col.E, lty=lty.E, lwd=lwd.E)
  polygon(NR$x, NR$y, border=border.NR, col=col.NR, lty=lty.NR, lwd=lwd.NR)
  M <- dim(li)[1]
  abline(h=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
  abline(v=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
  arrows(rep(0,M),rep(0,M),li[,1], li[,2], angle=angle.arrow,
         col=col.arrow,lwd=lwd.arrow, length=length.arrow)
  points(li[,1], li[,2], col=col.NR.pt, bg=col.NR.pos, pch=pch.NR.pos, cex=cex.NR.pos)
  if(!is.na(cex.NR.lab)){
    text(li[,1]*fac.NR.lab, li[,2]*fac.NR.lab, sp, col=col.NR.lab, font=font.sp, cex=cex.NR.lab)
  }
  if(isTRUE(leg)){
    filli <- c(col.E, col.NR,NA)
    borderi <- c(border.E,border.NR,NA)
    col.leg <- c(NA,NA, col.NR.pt)
    col.bg <- c(NA, NA,col.NR.pos)
    pch.leg <- c(NA,NA,pch.NR.pos)
    posi.cex <-c(NA,NA,cex.NR.pos)
    tex.leg <- c("E","NR", "NR position")

    if(anyNA(col.NR.pos)){
      pch.leg[3] <- NA
      col.leg[3] <- NA
      col.NR.pt[3] <- NA
      tex.leg[3] <- NA
    }
    lty.leg <- c(0,0,0)
    lwd.leg <- c(0,0,0)
    if(lty.E>1){
      pch.leg[1] <- NA
      lty.leg[1] <- lty.E
      lwd.leg[1] <- lwd.E
    }
    legend(posi.leg, legend=tex.leg,fill =filli, border=borderi, pch=pch.leg, col=col.leg, pt.cex = posi.cex,
           pt.bg=col.bg,lty=lty.leg,pt.lwd=c(NA,NA,1), lwd=lwd.leg, bty=bty.leg,...)
  }
}
