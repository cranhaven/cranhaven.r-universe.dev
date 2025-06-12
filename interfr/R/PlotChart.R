
#' plots the interference chart
#'@author Olivier Eterradossi, \email{olivier.eterradossi@mines-ales.fr}
#' @param IC Dataframe from \code{\link{InterferenceTable}}.
#' @param type Chart type (see Details).
#' @param Thickness If not NULL, a horizontal line is drawn at h= Thickness
#' (in micrometers).
#' @param x.lims Plotting range, horizontal axis (when NULL,
#' defaults to \code{c(0,0.05)} for Raith-Sorensen plots
#'  and to \code{c(0,2500)} for Michel-Levy plots).
#' @param radials If TRUE iso-birefringence lines will be plotted
#' (on Michel-Levy chart only).
#' @description
#' \code{PlotChart} plots the result of a call
#' to \code{\link{InterferenceTable}}
#'
#' @details
#' If \code{type} belongs to c("Sorensen","S","Raith-Sorensen","RS"), the
#' function plots interference colors on a grid with birefringence
#' as horizontal axis and thickness as vertical axis. If \code{type} belongs to
#' c("Michel-Levy","ML","MichelLevy","M"), the horizontal axis is retardation
#' (in nanometers) as in the classical Michel-Levy plot. When \code{radials} is
#' set to TRUE, birefringence appears as oblique lines with rounded values
#' printed at their end
#'
#' @return a Sorensen or Michel-Levy plot
#'
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics title
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom grDevices rgb
#' @importFrom stats lm
#' @importFrom stats coefficients
#' @references
#' Sorensen, B.E. (2013) A revised Michel-Levy interference colour chart based
#' on first-principles calculations. Eur. J. Mineral., 2013, 25, 5-10.
#' DOI:10.1127/0935-1221/2013/0025-2252 
#'
#' @examples
#'\dontrun{
#' PlotChart(IC=IC1,type="RS")
#' PlotChart(IC=IC1,type="ML")
#' PlotChart(IC=IC1,type="ML",radials=TRUE)
#' PlotChart(IC=IC1,type="ML",x.lims=range(IC1[,6]),Thickness = 35)
#'}
#'
#' @export

PlotChart<-function(IC,type="Sorensen",Thickness=30,x.lims=NULL,radials=FALSE){
  
  if (type %in% c("Sorensen","S","Raith-Sorensen","RS")) {
  if(is.null(x.lims)){x.lims=c(0,0.05)}
    graphics::plot(IC[,2],IC[,1],pch=15,col=grDevices::rgb(IC[,3:5]),cex=1.1,
    xlab="biref",ylab="thickness (microns)",xlim=x.lims)
    if(!is.null(Thickness)){graphics::abline(h=Thickness)}
    graphics::title(main="Computed Raith-Sorensen interference chart")
  }
  if (type %in% c("Michel-Levy","ML","MichelLevy","M")) {
  if(is.null(x.lims)){x.lims=c(0,2500)}
    graphics::plot(IC[,6],IC[,1],pch=15,col=grDevices::rgb(IC[,3:5]),
    cex=1.1,xlab="Path Difference (nm)",ylab="thickness (microns)",xlim=x.lims)
    if(!is.null(Thickness)){graphics::abline(h=Thickness)}
    graphics::title(main="Computed Michel-Levy interference chart")
    if(radials){subtab<-IC[IC[,1]==max(IC[,1],na.rm=TRUE),]
    tabs.biref<-seq(min(subtab[,2],na.rm=TRUE),max(subtab[,2],na.rm=TRUE),
    length.out=10)
    tabs.TT<-tabs.biref*unique(subtab[,1])*1000
    TT.infs<-which(tabs.TT<=x.lims[2])
    TT.sups<-which(tabs.TT>x.lims[2])
    graphics::segments(x0=rep(0,length(tabs.biref)),
    y0=rep(0,length(tabs.biref)),x1=tabs.TT,
    y1=rep(unique(subtab[,1]),length(tabs.biref)))
    graphics::text(y=unique(subtab[,1]),x=tabs.TT[TT.infs],
    round(tabs.biref[TT.infs],3),cex=0.6,col="white")
    for(k in 1:length(TT.sups)){
      xx<-c(0,tabs.TT[TT.sups][k])
      yy<-c(0,unique(subtab[,1]))
      regr<-stats::lm(yy~xx)
      yyy<-as.numeric(stats::coefficients(regr)[2])*x.lims[2]
      graphics::text(y=yyy,x=x.lims[2],round(tabs.biref[TT.sups],3)[k],
      cex=0.6,col="black")
    }
    }
  }
}
