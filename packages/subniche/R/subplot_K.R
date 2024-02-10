#' @title Plot sub-environmental space K in E
#' @aliases subplot_K
#' @description  The function to represent the sub-environment K in E.
#' @param subnic an object of class \code{subniche}.
#' @param main a main title for the plot, see \link[graphics]{title} for more details.
#' @param xlab label for x-axis, see \link[graphics]{title} for more details.
#' @param ylab label for y-axis, see \link[graphics]{title} for more details.
#' @param col.axis axis color, see \link[graphics]{par} for more details.
#' @param lty.axis axis line type, see \link[graphics]{par} for more details.
#' @param lwd.axis  axis width, see \link[graphics]{par} for more details.
#' @param border.E color border of E polygon, see \link[graphics]{polygon} for more details.
#' @param col.E inside color of E polygon, see \link[graphics]{polygon} for more details.
#' @param lty.E line type for the E border, see \link[graphics]{polygon} for more details.
#' @param lwd.E line width for the E border, see \link[graphics]{polygon} for more details.
#' @param border.K color border of K polygon, see \link[graphics]{polygon} for more details.
#' @param col.K inside color of K polygon, see \link[graphics]{polygon} for more details.
#' @param lty.K line type for the K border, see \link[graphics]{polygon} for more details.
#' @param lwd.K line width for the K border, see \link[graphics]{polygon} for more details.
#' @param col.Gk.pos color of the point representing Gk, see \link[graphics]{points} for more details.
#' @param col.Gk.pt point color contour if pch=21:25.
#' @param cex.Gk.pos size of the point representing Gk, see \link[graphics]{points} for more details.
#' @param pch.Gk.pos type of the point representing Gk, see \link[graphics]{points} for more details.
#' @param col.Gk.lab color of the Gk labels, see see \link[graphics]{text} for more details.
#' @param cex.Gk.lab size of the Gk labels defaults NA for no labels, see see \link[graphics]{text} for more details.
#' @param fac.Gk.lab factor for moving the Gk labels from its original coordinates for clarity, by defaults they are multiply 1.2
#' @param col.su color of the points representing the sampling units (SU), see \link[graphics]{points} for more details.
#' @param pt.su  point color contour if pch=21:25.
#' @param cex.su size of the points representing the sampling units (SU), see \link[graphics]{points} for more details.
#' @param pch.su type of the points representing the sampling units (SU), see \link[graphics]{points} for more details.
#' @param leg a logical option for legend to be plotted or not, default leg=T.
#' @param posi.leg legend location in the graph, see \link[graphics]{legend} for more details.
#' @param bty.leg the type of box to be drawn around the legends. The allowed values are "o" (the default) and "n". See \link[graphics]{legend} for more details
#' @param ...	further arguments passed to or from other methods.

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
#' #Plot K in E
#' subplot_K(subnic1)
#'
#' @rdname subplot_K
#' @export subplot_K
#' @importFrom graphics par layout arrows points legend polygon abline
#' @importFrom wordcloud textplot
#' @importFrom SIBER siberConvexhull
#'
#'
subplot_K <- function(subnic, main=NULL, xlab=NULL, ylab=NULL, col.axis="azure3", lty.axis=2, lwd.axis=2,
                      border.E="black",
                      col.E="#92c5de",
                      lty.E=1,
                      lwd.E=1,
                      border.K ="black",
                      col.K ="#2c7fb8",
                      lty.K=1,
                      lwd.K=1,
                      col.Gk.pos= "red",
                      col.Gk.pt= "black",
                      cex.Gk.pos=1,
                      pch.Gk.pos=21,
                      col.Gk.lab="black",
                      cex.Gk.lab= 0.8,
                      fac.Gk.lab=1.5,
                      col.su="#b35806",
                      pt.su="black",
                      cex.su=0.7,
                      pch.su=1,
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
  plot(subnic$ls, main=main, xlab=xlab, ylab=ylab, type="n",...)
  E <- siberConvexhull(subnic$ls[,1], subnic$ls[,2])
  polygon(E$xcoords,E$ycoords, border=border.E, col=col.E, lty=lty.E, lwd=lwd.E)
  for (i in 1:N){
    subnici <- subnic$ls[which(fac==lev[i]),]
    G_k <- subnic$G_k[grep(lev[i],rownames(subnic$G_k)),]
    K <- siberConvexhull(subnici[,1], subnici[,2])
    polygon(K$xcoords,K$ycoords, border=border.K, col=col.K, lty=lty.K, lwd=lwd.K)
    points(subnici,cex=cex.su, col=pt.su, bg=col.su, pch=pch.su)
    points(G_k[,1], G_k[,2], col=col.Gk.pt, bg=col.Gk.pos, pch=pch.Gk.pos, cex= cex.Gk.pos)
    if(!is.na(cex.Gk.lab)){
      text(G_k[,1]*fac.Gk.lab, G_k[,2]*fac.Gk.lab, lev[i], col=col.Gk.lab, cex=cex.Gk.lab)
    }
  }
  M <- nrow(subnic$c1)
  abline(h=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
  abline(v=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)

  if(isTRUE(leg)){
    filli <- c(col.E, col.K, NA,  NA)
    borderi <- c(border.E, border.K,  NA, NA)
    col.leg <- c(NA,NA, col.Gk.pt, pt.su)
    col.bg <- c(NA,NA, col.Gk.pos,col.su)
    pch.leg <- c(NA,NA,pch.Gk.pos,pch.su)
    tex.leg <- c("E","K","GK","SU")
    lty.leg <- c(0,0,0,0)
    lwd.leg <- c(0,0,0,0)
    posi.cex <-c(NA,NA,1,1)
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
             pt.bg=col.bg,lty=lty.leg,pt.lwd=c(NA,NA,1,1), lwd=lwd.leg, bty=bty.leg,...)

  }
}
