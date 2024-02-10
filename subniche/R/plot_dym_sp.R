#' @title Species subniches dynamic
#' @aliases plot_dym_sp
#' @description  The function represents the species' subniches SR within its realized niche NR.
#' @param subnic an object of class \code{subniche}.
#' @param sp a character string of the species name.
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
#' @param border.NR color border of NR polygon, see \link[graphics]{polygon} for more details.
#' @param col.NR inside color of NR polygon, see \link[graphics]{polygon} for more details.
#' @param lty.NR line type for the NR border, see \link[graphics]{polygon} for more details.
#' @param lwd.NR line width for the NR border, see \link[graphics]{polygon} for more details.
#' @param pch.NR.pos the type of points representing the NR position,  see \link[graphics]{points} for more details.
#' @param col.NR.pos the color of points representing the NR position, see \link[graphics]{points} for more details.
#' @param col.NR.pt point color contour if pch=21:25.
#' @param cex.NR.pos size of points representing the SR position, see \link[graphics]{points} for more details.
#' @param col.NR.lab color of the species label representing the NR position, see \link[wordcloud]{textplot} for more details.
#' @param cex.NR.lab size of the species label representing the NR position, see \link[wordcloud]{textplot} for more details.
#' @param fac.SR.lab factor for moving the SR labels from its original coordinates for clarity, by defaults they are multiply 1.2
#' @param border.SR color border of SR polygon, see \link[graphics]{polygon} for more details.
#' @param col.SR inside color of SR polygon, see \link[graphics]{polygon} for more details.
#' @param lty.SR line type for the SR border, see \link[graphics]{polygon} for more details.
#' @param lwd.SR line width for the SR border, see \link[graphics]{polygon} for more details.
#' @param col.SR.pt point color contour if pch=21:25.
#' @param col.SR.pos color of points representing the SR position, see \link[graphics]{points} for more details.
#' @param pch.SR.pos type of points representing the SR position, see \link[graphics]{points} for more details.
#' @param cex.SR.pos size of points representing the SR position, see \link[graphics]{points} for more details.
#' @param col.SR.lab color of the species label representing the SR position, see \link[graphics]{text} for more details.
#' @param cex.SR.lab size of the species label representing the SR position, see \link[graphics]{text} for more details.
#' @param col.axis axis color, see \link[graphics]{par} for more details.
#' @param lty.axis axis line type, see \link[graphics]{par} for more details.
#' @param lwd.axis  axis width, see \link[graphics]{par} for more details.
#' @param leg a logical option for legend to be plotted or not, default leg=T.
#' @param bty.leg the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n", see \link[graphics]{legend} for more details.
#' @param posi.leg setting legend positions with the following keywords "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center", see \link[graphics]{legend} for more details.
#' @param font.sp An integer which specifies which font to use for species label. 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic, see \link[graphics]{par} for more details.
#' @param ...	further arguments passed to or from other methods.
#' @rdname plot_dym_sp
#' @export plot_dym_sp
#' @details The convex hulls measured are :
#' \enumerate{
#' \item E is the environmental space.
#' \item NR the realized subniche.
#' \item SR the species realized subniche.
#' }
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
#' plot_dym_sp(subnic1, "Neba")
#' @importFrom graphics par layout arrows points legend polygon abline text
#' @importFrom wordcloud textplot
#' @importFrom SIBER siberConvexhull
plot_dym_sp <- function(subnic, sp, xlab=NULL, ylab=NULL, main=NA, col.axis="azure3", lty.axis=2, lwd.axis=2,
                        border.E="black",
                        col.E="#92c5de",
                        lty.E=1,
                        lwd.E=1,
                        col.NR ="#fdb462",
                        border.NR ="black",
                        lty.NR=1,
                        lwd.NR=1,
                        col.NR.lab="black",
                        cex.NR.lab=0.7,
                        pch.NR.pos= 21,
                        col.NR.pos="black",
                        col.NR.pt="black",
                        cex.NR.pos=1,
                        border.SR="black",
                        col.SR="#a1d99b",
                        lty.SR=1,
                        lwd.SR=1,
                        col.SR.lab="black",
                        cex.SR.lab=0.7,
                        fac.SR.lab=1.2,
                        pch.SR.pos= 21,
                        col.SR.pos="#ffa600",
                        col.SR.pt="black",
                        cex.SR.pos=1,
                        col.arrow="black",
                        angle.arrow=20,
                        lwd.arrow=2, length.arrow=0.1, font.sp=2, leg=T, posi.leg="topleft", bty.leg="n", ...){


fac <- subnic$factor
lev <- levels(fac)
ar_sub <- subarea(subnic)
eig <- round(subnic$eig/sum(subnic$eig)*100,2)[1:2]
E <- ar_sub$E
if(pch.SR.pos<21|pch.SR.pos>25){
  col.SR.pt <- col.SR.pos
}
if(pch.NR.pos<21|pch.NR.pos>25){
  col.NR.pt <- col.NR.pos
}
NR <- ar_sub$NR[[sp]]
if(is.null(xlab)){
  xlab=paste(paste("OMI1",eig[1], sep=" "),"%",sep="")}
if(is.null(ylab)){
  ylab=paste(paste("OMI2",eig[2], sep=" "),"%",sep="")}
plot(subnic$ls, main=main, xlab= xlab,
     ylab= ylab, type="n",...)
polygon(E$x, E$y, border=border.E, col=col.E, lty=lty.E, lwd=lwd.E)
polygon(NR$x,NR$y, border=border.NR, col=col.NR, lty=lty.NR,lwd= lwd.NR)

nami <- paste(sp, lev, sep="")
spli <- subnic$sub[rownames(subnic$sub)%in%nami,]
colnames(spli) <- colnames(subnic$li)
levi <- sub(sp,"",nami)
M <- length(levi)
if(is.na(col.NR.lab[M])){
  col.SRi <- rep(col.SR,M)
  border.SRi <- rep(border.SR,M)
}
for (j in 1:M){
  SR <- ar_sub$SR[[lev[j]]]
  SR <- SR[[nami[j]]]
    if (length(SR$x)>2){
      polygon(SR$x,SR$y, border=border.SRi[j], col=col.SRi[j],lty=lty.SR, lwd=lwd.SR)
    }
    else {
      points(SR$x,SR$y,pch=pch.SR.pos,col=col.SR.pt[j], bg=col.SR.pos[j], cex=cex.SR.pos)
    }
}
abline(h=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
abline(v=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
arrows(rep(0,M),rep(0,M),spli[,1], spli[,2], angle=angle.arrow,
       col=col.arrow,lwd=lwd.arrow, length=length.arrow)
  points(spli[,1], spli[,2], pch=pch.SR.pos, col=col.SR.pt, bg=col.SR.pos, cex=cex.SR.pos)
points(subnic$li[sp,1], subnic$li[sp,2], pch=pch.NR.pos, col=col.NR.pt, bg=col.NR.pos, cex=cex.NR.pos)

li <- rbind(subnic$li[sp,], spli)
if(anyNA(li)){
li <- li[-which(is.na(li)==T),]
}
if(sum(round(apply(li,2,diff),1))==0){
  text(subnic$li[sp,1]*fac.SR.lab,subnic$li[sp,2]*fac.SR.lab,sp, col=col.NR.lab, cex=cex.NR.lab, font = font.sp)
} else {
  li <- rbind(subnic$li[sp,]*fac.SR.lab, spli*fac.SR.lab)
 text(li[,1],li[,2],rownames(li),col=c(col.NR.lab,col.SR.lab), cex=c(cex.NR.lab,rep(cex.SR.lab,dim(spli)[1])), font = font.sp)
}

if(isTRUE(leg)){
  filli <- c(col.E, col.NR,NA,col.SR,NA)
  borderi <- c(border.E, border.NR,NA, border.SR,NA)
  col.leg <- c(NA,NA, col.NR.pt,NA,col.SR.pt)
  col.bg <- c(NA,NA, col.NR.pos,NA,col.SR.pos)
  pch.leg <- c(NA,NA,pch.NR.pos,NA,pch.SR.pos)
  tex.leg <- c("E","NR","NR position","SR","SR position")
  lty.leg <- c(0,0,NA,0,NA)
  lwd.leg <- c(0,0,NA,0,NA)
  posi.cex <-c(NA,NA,cex.NR.pos,NA,cex.SR.pos)

  if(is.na(col.E)){
    filli[1] <- NA
    borderi[1] <- NA
    tex.leg[1] <- NA
    }
  if(is.na(col.NR)){
    filli[2] <- NA
    borderi[2] <- NA
    tex.leg[2] <- NA
    }
  if(anyNA(col.SR)){
    filli[4] <- NA
    borderi[4] <- NA
    tex.leg[4] <- NA
  }
  if(anyNA(cex.NR.pos)){
    posi.cex[3] <- NA
    tex.leg[3] <- NA
  }
  if(anyNA(cex.SR.pos)){
    posi.cex[5] <- NA
    tex.leg[5] <- NA
  }
  if(lty.E>1){
    pch.leg[1] <- NA
    lty.leg[1] <- lty.E
    lwd.leg[1] <- lwd.E
    }
  if (lty.NR>1){
    pch.leg[2] <- NA
    lty.leg[2] <- lty.NR
    lwd.leg[2] <- lwd.NR
    }
  if (lty.SR>1){
    pch.leg[4] <- NA
    lty.leg[4] <- lty.SR
    lwd.leg[4] <- lwd.SR
    }
  legend(posi.leg, legend=tex.leg, fill =filli, border=borderi, pch=pch.leg, col=col.leg, pt.cex = posi.cex,
         pt.bg=col.bg, lty=lty.leg, pt.lwd=c(NA,NA,1,NA,1), lwd=lwd.leg, bty=bty.leg,...)
  }
}
