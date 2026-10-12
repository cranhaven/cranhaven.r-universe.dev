#' Tukey Nonadditivity Plot for Two-way ANOVA
#'
#' @param formula A formula for a two-way ANOVA in the form Response=FactorA+FactorB (or FactorA*FactorB)
#' @param data A dataframe
#' @param out Control what is returned. Default is "n"=nothing. Other options are "comp" for the comparisons, "line" for the equation of the line, and "resid" for the cell residuals.
#' @param main Add a title, default is "Tukey Nonadditivity Plot"
#' @param ylab Label vertical axis, default is "Residuals"

#' @import stats
#'
#' @return Depends on the option set with \code{out}.
#'
#' @description
#' This function produces a Tukey nonadditivity plot for a two-way ANOVA model.
#'
#' @details
#' More details need to be written
#'
#' @examples
#' data(Dinosaurs)
#' TukeyNonaddPlot(Iridium~Source*factor(Depth),data=Dinosaurs)
#'
#' @export

TukeyNonaddPlot=function(formula,data,out="n",main="Tukey Nonadditivity Plot",ylab="Residuals"){
  mod=aov(formula,data)
  newdata=mod$model
  names(newdata)=c("Y","A","B")
  newdata$A=factor(newdata$A); newdata$B=factor(newdata$B)
  I=nlevels(newdata$A)
  J=nlevels(newdata$B)
  cellmeans=tapply(newdata$Y,list(newdata$A,newdata$B),mean)
  GrandMean=mean(cellmeans)
  RowEffects=rep(rowMeans(cellmeans)-GrandMean,J)
  ColEffects=rep(colMeans(cellmeans)-GrandMean,each=I)
  Comparisons=RowEffects*ColEffects/GrandMean
  CellResid=as.vector(cellmeans)-(RowEffects+ColEffects+GrandMean)

  plot(CellResid~Comparisons,ylab=ylab)
  modline=lm(CellResid~Comparisons)
  abline(modline)
  text(0,min(CellResid),paste("slope=",round(modline$coeff[2],2)),col="blue")
  if(out=="comp"){return(Comparisons)}
  if(out=="line"){return(modline)}
  if(out=="resid"){return(CellResid)}
}
