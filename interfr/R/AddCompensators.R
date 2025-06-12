#' Shows the color changes when using sensible and quarter compensators 
#'
#'@author Olivier Eterradossi, \email{olivier.eterradossi@mines-ales.fr}
#'
#' @param loc A named list with members x and y , or an interactive selection.
#' @param type Chart type (Raith-Sorensen or Michel-Levy),
#' see \code{\link{PlotChart}}.
#' @param quarter Numeric, the retardation of the quarter slab (in nanometers).
#' @param sensible Numeric, the retardation of the lambda slab (in nanometers).
#' @description
#' \code{AddCompensators} takes coordinates of one point on the interference
#' chart (either provided as a named list or interactively selected on
#' the chart using \code{locator}) and adds five circles to the plot: 
#' a black one centered on the selected interference color, and
#' four that correspond to adding or substracting the retardations
#' of a quarter plate (grey circles) and a sensible plate (red circles).
#'
#' @return Called for its side effect of adding circles to an existing plot
#' @examples
#'\dontrun{
#' PlotChart(IC=IC1,type="RS",x.lims=range(IC1[,2]),Thickness = 4000)
#' AddCompensators()
#' # the same without interactive selection:
#' PlotChart(IC=IC1,type="RS",x.lims=range(IC1[,2]),Thickness = 4000)
#' AddCompensators(loc=list("x"=0.0002566569,"y"=3999.757))
#'}
#' @importFrom graphics locator
#' @importFrom graphics par
#' @importFrom plotrix draw.circle
#' @seealso \code{\link[graphics]{locator}},
#' \code{\link[interfr]{PlotChart}}


AddCompensators<-function(loc=locator(1),type="RS",quarter=147.3,sensible=530){
  limits<-par("usr")
  if (type %in% c("Sorensen","S","Raith-Sorensen","RS")) {
    thickness<-1000*loc$y
    biref<-loc$x
    plotrix::draw.circle(x=loc$x,y=loc$y,radius=0.01*(limits[2]-limits[1]))
    
    TT.obs<-thickness*biref
    
    TT.minus.025.lambda<-TT.obs-quarter
    TT.plus.025.lambda<-TT.obs+quarter
    TT.minus.lambda<-TT.obs-sensible
    TT.plus.lambda<-TT.obs+sensible
    
    D.minus.025.lambda<-TT.minus.025.lambda/thickness
    D.plus.025.lambda<-TT.plus.025.lambda/thickness
    D.minus.lambda<-TT.minus.lambda/thickness
    D.plus.lambda<-TT.plus.lambda/thickness
    
    plotrix::draw.circle(x=D.minus.025.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="grey50")
    plotrix::draw.circle(x=D.plus.025.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="grey50")
    plotrix::draw.circle(x=D.minus.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="firebrick")
    plotrix::draw.circle(x=D.plus.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="firebrick")
  }
  
  if (type %in% c("Michel-Levy","ML","MichelLevy")) {
    thickness<-1000*loc$y
    TT.obs<-loc$x
    plotrix::draw.circle(x=loc$x,y=loc$y,radius=0.01*(limits[2]-limits[1]))
    
    
    TT.minus.025.lambda<-TT.obs-quarter
    TT.plus.025.lambda<-TT.obs+quarter
    TT.minus.lambda<-TT.obs-sensible
    TT.plus.lambda<-TT.obs+sensible
    
    plotrix::draw.circle(x=TT.minus.025.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="grey50")
    plotrix::draw.circle(x=TT.plus.025.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="grey50")
    plotrix::draw.circle(x=TT.minus.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="firebrick")
    plotrix::draw.circle(x=TT.plus.lambda,y=loc$y,
    radius=0.01*(limits[2]-limits[1]),border="firebrick")
  }
}
