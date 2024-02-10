#' @title Sub-community plot under each sub-environmental space K
#' @aliases subplot
#' @description  The function to represent the community subniche position under each subenvironment K with their respective marginality from Gk.
#' @param ...	further arguments passed to or from other methods.
#' @param subnic an object of class \code{subniche}.
#' @param sig_thres value for minimum significance, default 0.05
#' @param sig a factor defining the significance species, default NULL.
#' @param main a main title for the plot, see \link[graphics]{title} for more details.
#' @param xlab a label for the x axis, defaults to a description of x, see \link[graphics]{title} for more details.
#' @param ylab a label for the y axis, defaults to a description of y, see \link[graphics]{title} for more details.
#' @param col.axis axis color, see \link[graphics]{par} for more details.
#' @param lty.axis axis line type, see \link[graphics]{par} for more details.
#' @param lwd.axis  axis width, see \link[graphics]{par} for more details.
#'
#' @param pch.SR.pos type of the point representing SR position, see \link[graphics]{points} for more details.
#' @param cex.SR.pos size of the point representing SR position, see \link[graphics]{points} for more details.
#' @param col.SR.pt  point color contour if pch=21:25.
#' @param col.SR.pos color of the point representing SR position, see \link[graphics]{points} for more details.
#' @param col.SR.lab color of the species labels, see see \link[graphics]{text} for more details.
#' @param cex.SR.lab size of the species labels defautls NA for no labels, see see \link[graphics]{text} for more details.
#' @param fac.SR.lab factor for moving the SR labels from its original coordinates for clarity, by defaults they are multiply 1.2

#' @param border.E color border of E polygon, see \link[graphics]{polygon} for more details.
#' @param col.E inside color of E polygon, see \link[graphics]{polygon} for more details.
#' @param lty.E line type for the E border, see \link[graphics]{polygon} for more details.
#' @param lwd.E line width for the E border, see \link[graphics]{polygon} for more details.
#'
#' @param border.K color border of K polygon, see \link[graphics]{polygon} for more details.
#' @param col.K inside color of K polygon, see \link[graphics]{polygon} for more details.
#' @param lty.K line type for the K border, see \link[graphics]{polygon} for more details.
#' @param lwd.K line width for the K border, see \link[graphics]{polygon} for more details.
#'
#' @param col.Gk.pos color of the point representing Gk, see \link[graphics]{points} for more details.
#' @param col.Gk.pt point color contour if pch=21:25.
#' @param cex.Gk.pos size of the point representing Gk, see \link[graphics]{points} for more details.
#' @param pch.Gk.pos type of the point representing Gk, see \link[graphics]{points} for more details.
#'
#' @param col.su color of the points representing the sampling units (SU), see \link[graphics]{points} for more details.
#' @param pt.su  point color contour if pch=21:25.
#' @param cex.su size of the points representing the sampling units (SU), see \link[graphics]{points} for more details.
#' @param pch.su type of the points representing the sampling units (SU), see \link[graphics]{points} for more details.
#'
#' @param leg a logical option for legend to be plotted or not, default leg=T.
#' @param font.sp font of the species labels, see see \link[graphics]{text} for more details.
#' @param posi.leg legend location in the graph, see \link[graphics]{legend} for more details.
#' @param col.arrow arrow color, see \link[graphics]{arrows} for more details.
#' @param angle.arrow arrow angle head, see \link[graphics]{arrows} for more details.
#' @param lwd.arrow arrow width, see \link[graphics]{arrows} for more details.
#' @param length.arrow arrow head length, see \link[graphics]{arrows} for more details.
#' @param bty.leg the type of box to be drawn around the legends. The allowed values are "o" (the default) and "n". See \link[graphics]{legend} for more details

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
#' #Two graphs are drawn one after the other
#' siggk <- rtestsubor(subnic1,10)
#' sig = c(siggk$`1`$witomigktest$subni.pvalue[-28],siggk$`2`$witomigktest$subni.pvalue[-28])
#' subplot(subnic1, sig = sig, sig_thres= 0.1)
#'
#' @rdname subplot
#' @export subplot
#' @importFrom graphics par layout arrows points legend polygon abline
#' @importFrom wordcloud textplot
#' @importFrom SIBER siberConvexhull
subplot <- function(subnic, main=NULL,sig=NULL, sig_thres=0.05, xlab=NULL, ylab=NULL, col.axis="azure3", lty.axis=2, lwd.axis=2,
                    pch.SR.pos=21,
                    cex.SR.pos=1,
                    col.SR.pt="black",
                    col.SR.pos="#ffa600",
                    col.SR.lab="black",
                    cex.SR.lab= NA,
                    fac.SR.lab=1.2,
                    border.E="black",
                    col.E="#92c5de",
                    lty.E=1,
                    lwd.E=1,
                    border.K ="black",
                    col.K ="#2c7fb8",
                    lty.K=1,
                    lwd.K=1,
                    col.arrow="black",
                    angle.arrow=20,
                    lwd.arrow=2,
                    length.arrow=0.1,
                    col.Gk.pos= "red",
                    col.Gk.pt= "black",
                    cex.Gk.pos=1,
                    pch.Gk.pos=21,
                    col.su="#b35806",
                    pt.su="black",
                    cex.su=0.7,
                    pch.su=1,
                    font.sp=2,
                    leg=T,
                    posi.leg="topleft",
                    bty.leg="n", ...){
  fac <- subnic$factor
  lev <- levels(fac)
  eig <- round(subnic$eig/sum(subnic$eig)*100,2)[1:2]
  if(is.null(xlab)){
    xlab=paste(paste("OMI1",eig[1], sep=" "),"%",sep="")}
  if(is.null(ylab)){
    ylab=paste(paste("OMI2",eig[2], sep=" "),"%",sep="")}
  N <- length(lev)
  subsp <- subnic$sub
  if(pch.SR.pos<21|pch.SR.pos>25){
    col.SR.pt <- col.SR.pos
  }
  if(pch.Gk.pos<21|pch.Gk.pos>25){
    col.Gk.pt <- col.Gk.pos
  }
  if(pch.su<21|pch.su>25){
    pt.su <- col.su
  }
  for (i in 1:N){
    subnici <- subnic$ls[which(fac==lev[i]),]
    G_k <- subnic$G_k[grep(lev[i],rownames(subnic$G_k)),]
    if(is.null(sig)){
      subspk <- subsp[grep(lev[i],rownames(subsp)),]
      subspk <- subspk[!is.na(subspk[,1]),]
    }else{
      subspk <- subsp[which(round(sig,2)<=sig_thres),]
      subspk <- subspk[grep(lev[i],rownames(subspk)),]
      subspk <- subspk[!is.na(subspk[,1]),]
    }
    sp <- sub(lev[i],"",rownames(subspk))
    m <- dim(subspk)[1]
    plot(subnic$ls, main=main, xlab=xlab, ylab=ylab, type="n",...)
    E <- siberConvexhull(subnic$ls[,1], subnic$ls[,2])
    polygon(E$xcoords,E$ycoords, border=border.E, col=col.E, lty=lty.E, lwd=lwd.E)
    K <- siberConvexhull(subnici[,1], subnici[,2])
    polygon(K$xcoords,K$ycoords, border=border.K, col=col.K, lty=lty.K, lwd=lwd.K)
    abline(h=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
    abline(v=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
    arrows(rep(G_k[,1],m),rep( G_k[,2],m),subspk[,1], subspk[,2], angle=angle.arrow,
           col=col.arrow,lwd=lwd.arrow, length=length.arrow)
    points(subnici,cex=cex.su, col=pt.su, bg=col.su, pch=pch.su)
    points(G_k[,1], G_k[,2], col=col.Gk.pt, bg=col.Gk.pos, pch=pch.Gk.pos, cex= cex.Gk.pos)
    points(subspk[,1], subspk[,2], col=col.SR.pt, bg=col.SR.pos, pch=pch.SR.pos, cex= cex.SR.pos)
    if(!is.na(cex.SR.lab)){
    text(subspk[,1]*fac.SR.lab, subspk[,2]*fac.SR.lab, sp, col=col.SR.lab, font=font.sp, cex=cex.SR.lab)
    }
    if(isTRUE(leg)){
      filli <- c(col.E, col.K, NA, NA, NA)
      borderi <- c(border.E, border.K, NA, NA, NA)
      col.leg <- c(NA,NA, col.Gk.pt, col.SR.pt, pt.su)
      col.bg <- c(NA,NA, col.Gk.pos,col.SR.pos,col.su)
      pch.leg <- c(NA,NA,pch.Gk.pos,pch.SR.pos,pch.su)
      tex.leg <- c("E",paste("K", lev[i], sep=""),paste("GK", lev[i], sep=""),"SR","SU")
      lty.leg <- c(0,0,0,0,0)
      lwd.leg <- c(0,0,0,0,0)
      posi.cex <-c(NA,NA,1,1,1)

      if(is.na(col.E)){
        filli[1] <- NA
        borderi[1] <- NA
        tex.leg[1] <- NA
      }
      if(is.na(col.K)){
        filli[2] <- NA
        borderi[2] <- NA
        tex.leg[2] <- NA
      }

      if(anyNA(cex.Gk.pos)){
        posi.cex[3] <- NA
        tex.leg[3] <- NA
      }
      if(anyNA(cex.SR.pos)){
        posi.cex[3] <- NA
        tex.leg[3] <- NA
      }
      if(anyNA(cex.su)){
        posi.cex[3] <- NA
        tex.leg[3] <- NA
      }
      if(lty.E>1){
        pch.leg[1] <- NA
        lty.leg[1] <- lty.E
        lwd.leg[1] <- lwd.E
      }
      if(lty.K>1){
        pch.leg[2] <- NA
        lty.leg[2] <- lty.E
        lwd.leg[2] <- lwd.E
      }


      legend(posi.leg, legend=tex.leg,fill =filli, border=borderi, pch=pch.leg, col=col.leg, pt.cex = posi.cex,
             pt.bg=col.bg,lty=lty.leg,pt.lwd=c(NA,NA,1,1,1), lwd=lwd.leg, bty=bty.leg,...)

    }
  }
}
