#' @title Plot a species subniche under each sub-environmental space K
#' @aliases subplot_sp
#' @description  The function to represent the species subniche under each subenvironment K with their respective marginality from G_K.
#' @param ...	 further arguments passed to or from other methods.
#' @param subnic an object of class \code{subniche}.
#' @param sp a character string of the species name.
#' @param main a main title for the plot, see \link[graphics]{title} for more details.
#' @param col.axis axis color, see \link[graphics]{par} for more details.
#' @param xlab label for x-axis, see \link[graphics]{title} for more details.
#' @param ylab label for y-axis, see \link[graphics]{title} for more details.
#' @param lty.axis axis line type, see \link[graphics]{par} for more details.
#' @param lwd.axis  axis width, see \link[graphics]{par} for more details.

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
#' @param col.Gk.pos color of the point representing G_k, see \link[graphics]{points} for more details.
#' @param col.Gk.pt point color contour if pch=21:25.
#' @param cex.Gk.pos size of the point representing G_k, see \link[graphics]{points} for more details.
#' @param pch.Gk.pos type of the point representing G_k, see \link[graphics]{points} for more details.
#'
#' @param border.SP color border of species subniche polygon, see \link[graphics]{polygon} for more details.
#' @param lty.SP line type for the SP border, see \link[graphics]{polygon} for more details.
#' @param lwd.SP line width for the SP border, see \link[graphics]{polygon} for more details.
#' @param col.SB color of the SB area.
#'
#' @param border.NR color border of NR polygon, see \link[graphics]{polygon} for more details.
#' @param col.NR inside color of NR polygon, see \link[graphics]{polygon} for more details.
#' @param lty.NR line type for the NR border, see \link[graphics]{polygon} for more details.
#' @param lwd.NR line width for the NR border, see \link[graphics]{polygon} for more details.
#'
#' @param border.SR color border of SR polygon, see \link[graphics]{polygon} for more details.
#' @param col.SR inside color of SR polygon, see \link[graphics]{polygon} for more details.
#' @param lty.SR line type for the SR border, see \link[graphics]{polygon} for more details.
#' @param lwd.SR line width for the SR border, see \link[graphics]{polygon} for more details.
#' @param col.SR.pos color of points representing the SR position, see \link[graphics]{points} for more details.
#' @param col.SR.pt point color contour if pch=21:25.
#' @param pch.SR.pos type of points representing the SR position, see \link[graphics]{points} for more details.
#' @param cex.SR.pos size of points representing the SR position, see \link[graphics]{points} for more details.
#' @param col.SR.lab color of the species label representing the SR position, see \link[graphics]{text} for more details.
#' @param cex.SR.lab size of the species label representing the SR position, see \link[graphics]{text} for more details.
#' @param fac.SR.lab factor for moving the SR labels from its original coordinates for clarity, by defaults they are multiply 1.2
#' @param font.sp An integer which specifies which font to use for species label. 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic, see \link[graphics]{par} for more details.
#' @param leg a logical option for legend to be plotted or not, default leg=T.
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
#' eig <- round(subnic1$eig/sum(subnic1$eig)*100,2)[1:2]
#' #Two graphs are drawn one after the other
#' subplot_sp(subnic1,"Neba")
#' @rdname subplot_sp
#' @export
#' @importFrom graphics par layout arrows points legend polygon abline
#' @importFrom wordcloud textplot
#' @importFrom SIBER siberConvexhull
subplot_sp <- function(subnic, sp, main=NULL, col.axis="azure3", lty.axis=2, lwd.axis=2,  xlab=NULL, ylab=NULL,
                       border.E="black",
                       col.E="#92c5de",
                       lty.E=1,
                       lwd.E=1,
                       border.K ="black",
                       lwd.K=1,
                       col.K ="#2c7fb8",
                       lty.K=1,
                       col.Gk.pos= "red",
                       col.Gk.pt= "black",
                       cex.Gk.pos=1,
                       pch.Gk.pos=21,
                       border.SP="#bc5090",
                       col.SB="#ffff99",
                       lty.SP=1,
                       lwd.SP=2,
                       border.NR ="#fdb462",
                       col.NR = NA,
                       lty.NR=1,
                       lwd.NR=2,
                       border.SR="#a1d99b",
                       col.SR="#a1d99b",
                       lty.SR=1,
                       lwd.SR=1,
                       pch.SR.pos=19,
                       cex.SR.pos=1,
                       col.SR.pt="black",
                       col.SR.pos="black",
                       cex.SR.lab=0.7,
                       col.SR.lab="black",
                       fac.SR.lab=1.2,
                       font.sp=2,
                       col.arrow="black",
                       angle.arrow=20,
                       lwd.arrow=2,
                       length.arrow=0.1,
                       leg=T,
                       posi.leg="topleft",
                       bty.leg="n", ...){
  fac <- subnic$factor
  lev <- levels(fac)
  N <- length(lev)
  are_sub <- subarea(subnic)
  if(pch.SR.pos<21|pch.SR.pos>25){
    col.SR.pt <- col.SR.pos
  }
  if(pch.Gk.pos<21|pch.Gk.pos>25){
    col.Gk.pt <- col.Gk.pos
  }
  eig <- round(subnic$eig/sum(subnic$eig)*100,2)[1:2]
  nami <- paste(sp, lev, sep="")
  subsp <- subnic$sub[rownames(subnic$sub)%in%nami,]
  nami <- rownames(subsp)[which(is.na(subsp[,1])==F)]
  subsp <- subsp[which(is.na(subsp[,1])==F),]
  levi <- sub(sp,"",nami)
  M <- length(levi)
    if(is.null(xlab)){
    xlab=paste(paste("OMI1",eig[1], sep=" "),"%",sep="")}
    if(is.null(ylab)){
    ylab=paste(paste("OMI2",eig[2], sep=" "),"%",sep="")}
    for (i in 1:M){
      plot(subnic$ls, main=main,  xlab=xlab, ylab=ylab, type="n",...)
      E <- are_sub$E
      K <- are_sub$K[[levi[i]]]
      NR <- are_sub$NR[[sp]]
      SR <- are_sub$SR[[levi[i]]][[nami[i]]]
      SP <- are_sub$SP[[levi[i]]][[nami[i]]]
      polygon(E$x,E$y, border=border.E, col=col.E, lty=lty.E, lwd=lwd.E)
      polygon(K$x,K$y, border=border.K, col=col.K, lty=lty.K, lwd=lwd.K)
      polygon(NR$x,NR$y, border=border.NR, col=col.NR, lty=lty.NR, lwd=lwd.NR)
      polygon(SP$x,SP$y, border=border.SP, col=col.SB, lty=lty.SP, lwd=lwd.SP)
      polygon(SR$x,SR$y, border=border.SR, col=col.SR, lty=lty.SR, lwd=lwd.SR)
      G_k <- subnic$G_k[grep(lev[i],rownames(subnic$G_k)),]
      abline(h=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
      abline(v=0, lty=lty.axis, lwd=lwd.axis, col=col.axis)
      arrows(G_k[,1],G_k[,2],subsp[nami[i],1], subsp[nami[i],2], angle=angle.arrow,
           col=col.arrow,lwd=lwd.arrow, length=length.arrow)
      points(G_k[,1], G_k[,2], col=col.Gk.pt, bg=col.Gk.pos, pch=pch.Gk.pos, cex= cex.Gk.pos)
      points(subsp[nami[i],1], subsp[nami[i],2], pch=pch.SR.pos, col=col.SR.pt, bg=col.SR.pos, cex=cex.SR.pos)
      text(subsp[nami[i],1]*fac.SR.lab, subsp[nami[i],2]*fac.SR.lab,nami[i], col=col.SR.lab, cex=cex.SR.lab, font=font.sp)

      if(isTRUE(leg)){
        filli <- c(col.E, col.K, NA, col.NR, NA, col.SR, col.SB)
        borderi <- c(border.E, border.K, NA, border.NR, border.SP, border.SR, NA)
        col.leg <- c(NA,NA, col.Gk.pt,NA,NA,NA,NA)
        col.bg <- c(NA,NA, col.Gk.pos,NA,NA,NA,NA)
        pch.leg <- c(NA,NA,pch.Gk.pos,NA,NA,NA,NA)
        tex.leg <- c("E",paste("K", levi[i], sep=""),paste("GK", levi[i], sep=""),"NR","SP","SR","SB")
        lty.leg <- c(0,0,0,0,0,0,0)
        lwd.leg <- c(0,0,0,0,0,0,0)
        posi.cex <-c(NA,NA,cex.Gk.pos,NA,NA,NA,NA)

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
        if(is.na(border.NR)){
          filli[4] <- NA
          borderi[4] <- NA
          tex.leg[4] <- NA
        }
        if(anyNA(border.SP)){
          filli[5] <- NA
          borderi[5] <- NA
          tex.leg[5] <- NA
        }
        if(anyNA(col.SR)){
          filli[6] <- NA
          borderi[6] <- NA
          tex.leg[6] <- NA
        }
        if(anyNA(col.SB)){
          filli[7] <- NA
          borderi[7] <- NA
          tex.leg[7] <- NA
        }
        if(anyNA(cex.Gk.pos)){
          posi.cex[2] <- NA
          tex.leg[2] <- NA
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
        if (lty.NR>1){
          pch.leg[4] <- NA
          lty.leg[4] <- lty.NR
          lwd.leg[4] <- lwd.NR
        }
        if (lty.SP>1){
          pch.leg[5] <- NA
          lty.leg[5] <- lty.SR
          lwd.leg[5] <- lwd.SR
        }
        if (lty.SR>1){
          pch.leg[6] <- NA
          lty.leg[6] <- lty.SR
          lwd.leg[6] <- lwd.SR
        }

        legend(posi.leg, legend=tex.leg,fill =filli, border=borderi, pch=pch.leg, col=col.leg, pt.cex = posi.cex,
               pt.bg=col.bg,lty=lty.leg,pt.lwd=c(NA,NA,1,NA,NA,NA,NA), lwd=lwd.leg, bty=bty.leg,...)
      }
    }
}
