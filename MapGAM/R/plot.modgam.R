#***********************************************************************************
#
# Maps Predicted Values and Clusters for modgam Objects
# Copyright (C) 2016, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************
plot.modgam <- function(x, map = NULL, exp = FALSE, add = FALSE, intervals=TRUE, mapmin = NULL, 
                        mapmax = NULL, col.seq = diverge_hsv(201), anchor=FALSE, border.gray = 0.3, 
                        contours=c("none","response","permrank","interval"), 
                        contours.drawlabels=FALSE, contours.lty=1, contours.lwd=1, contours.levels, 
                        contours.labcex=0.7, arrow=TRUE, axes=FALSE, ptsize=0.9, alpha=0.05, 
                        mai, legend.name = "predicted values",legend.cex=1,...)
{
  modgamobj = x
  leglab = legend.name
  if(missing(legend.name)){
    if(!is.null(modgamobj$family)){
      if(modgamobj$family[1]=="survival") leglab = if(exp) "hazard ratio" else "log hazard ratio"
      if(modgamobj$family[1]=="binomial"&modgamobj$family[2]=="logit") leglab = if(exp) "odds ratio" else "log odds ratio"
      if(modgamobj$family[1]=="poisson"&modgamobj$family[2]=="log") leglab = if(exp) "risk ratio" else "log risk ratio"
    }
  }
  contours = contours[1]
  
  ## contour settings
  if(contours!="none"){
    if (contours=="permrank" && is.null(modgamobj$pointwise.permt)) {
      warning("permrank contours omitted because permute=0 or pointwise=FALSE in modgam")
      contours = "none"
    }
    if (contours=="interval" && any(c(is.null(modgamobj$conf.low),is.null(modgamobj$conf.high)))){
      warning("interval contours omitted because no conf.low or conf.high in modgam")
      contours = "none"
    }
    if(!is.element(contours,c("none","response","permrank","interval"))){
      warning("contours omitted because contours type not recognized")
      contours = "none"
    }
    if(contours == "response"){
      contours = "fit"
      if(missing(contours.drawlabels)) contours.drawlabels = TRUE
    }
    if(contours == "permrank"){
      contours = "pointwise.permt"
      if(missing(contours.levels)) contours.levels = c(alpha/2, 1-alpha/2)
      if(missing(contours.lwd)) contours.lwd = 2
      if(missing(contours.lty)) contours.lty = 1
    }
    if(contours == "interval"){
      if(missing(contours.levels)) contours.levels = c(-1,1)
      if(missing(contours.lwd)) contours.lwd = 2
      if(missing(contours.lty)) contours.lty = 1
    }
  }
  legend.add.line = if(exp) 1 else 0
  if(intervals & all(c(!is.null(modgamobj$conf.low),!is.null(modgamobj$conf.high)))){
    ## Plot the confidence bands as well as predictions by default
    if(is.null(mapmin))
      mapmin=min(if(exp)modgamobj$exp.conf.low else modgamobj$conf.low,rm.na=TRUE)
    if(is.null(mapmax))
      mapmax=max(if(exp)modgamobj$exp.conf.high else modgamobj$conf.high,rm.na=TRUE)
    mmai = if(missing(mai)) c(0,0,0.3,0) else mai
    legend.cex = legend.cex*1.4
    op.mfrow = par()$mfrow
    tempobj1 = modgamobj
    tempobj1$fit = modgamobj$conf.low; tempobj1$exp.fit = modgamobj$exp.conf.low
    tempobj2 = modgamobj
    tempobj2$fit = modgamobj$conf.high; tempobj2$exp.fit = modgamobj$exp.conf.high
    par(mfrow = c(1,3))
    colormap(tempobj1, map, exp, add, mapmin, mapmax, col.seq, anchor, border.gray,contours, contours.drawlabels, contours.lty,
             contours.lwd, contours.levels, contours.labcex, 0, arrow, axes, ptsize,mmai,leglab,legend.cex, legend.add.line,
             ...)
    title(main=paste(round((1-modgamobj$predobj$level)*100,2),"% CI (lower)"),cex.main=legend.cex)
    colormap(modgamobj, map, exp, add, mapmin, mapmax, col.seq, anchor, border.gray,contours, contours.drawlabels, contours.lty,
             contours.lwd, contours.levels, contours.labcex, 0, arrow, axes, ptsize,mmai,leglab,legend.cex, legend.add.line,
             ...)
    title(main="Point Estimate",cex.main=legend.cex)
    colormap(tempobj2, map, exp, add, mapmin, mapmax, col.seq, anchor, border.gray,contours, contours.drawlabels, contours.lty,
             contours.lwd, contours.levels, contours.labcex, 0, arrow, axes, ptsize,mmai,leglab,legend.cex, legend.add.line,
             ...)
    title(main=paste(round((1-modgamobj$predobj$level)*100,2),"% CI (higher)"),cex.main=legend.cex)
    par(mfrow=op.mfrow)
  }else
    ## Plot the predictions only
    colormap(modgamobj, map, exp, add, mapmin, mapmax, col.seq, anchor, border.gray,contours, contours.drawlabels, contours.lty,
             contours.lwd, contours.levels, contours.labcex,0, arrow, axes, ptsize, mai,leglab,legend.cex, legend.add.line,
             ...)
}