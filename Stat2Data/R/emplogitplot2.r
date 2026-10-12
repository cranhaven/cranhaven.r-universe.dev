#' Empirical logit plot for one quantitative variable by categorical groups
#'
#' @param formula A formula of the form (binary) Response~Quantitative Predictor+Factor
#' @param data A dataframe
#' @param ngroups Number of groups to use (not needed if breaks is used), ngroups="all" uses all unique values
#' @param breaks A vector of endpoints for the bins (not needed if ngroups is used)
#' @param yes Set a value for the response to be counted for proportions (optional)
#' @param padj Should proportions be adjusted to avoid zero and one? (default is TRUE)
#' @param out Should the function return a dataframe with group and factor information? (default is FALSE)
#' @param showplot Show the plot? default is TRUE
#' @param showline Show the regression lines? default is TRUE
#' @param ylab Text label for the vertical axis (default is "Log(Odds)")
#' @param xlab Text label for the horizontal axis (default is NULL)
#' @param putlegend Position for the legend (default is "n" for no legend)
#' @param levelcol Vector of colors for the factor levels
#' @param pch Plot character for the dots
#' @param main Title for plot
#' @param ylim Limits for the vertical axis
#' @param xlim Limits for the horizontal axis
#' @param lty Line type (default is 1)
#' @param lwd Line width (default is 1)
#' @param cex Multiplier for plot symbols

#'
#' @return A dataframe with group information (if out=TRUE)
#'
#' @description
#' This function produces an empirical logit plot for a binary response variable and with a
#' single quantitative predictor variable broken down by a single categorical factor.
#'
#' @details
##' Values of the quantitative explanatory variable will be grouped into \code{ngroups}
#' roughly equal sized groups, unless \code{breaks} is used to determine the boundaries of the groups.
#' Using \code{ngroups="all"} will make each distinct value of the explanatory variable its own group \cr
#'
#' We find a proportion for the binary response variable within each of the groups created from
#' the quantitative variable crossed with the categrical variable. To avoid problems with proportions
#' of zero and one, we compute an adjusted proportion with
#' \code{(Number yes +0.5)/(Number of cases+1)}. This is converted to an adjusted log odds
#' \code{log(adjp/(1-adjp))}.  What constitutes a "success" can be specified with \code{yes=}
#' and the proportion adjustment can be turned off (if no group proportions are likely to be zero or one)
#' with \code{padj=FALSE}.\cr
#'
#' The function plots the log odds versus the mean of the explanatory variable within each group with different
#' colors for each of the categories defined by the categorical variable.
#'  A least square line is fit to these points within each categorical group.
#'  The plot can be suppressed with \code{showplot=FALSE}.\cr
#'
#'  The \code{out=TRUE} option will return a dataframe with the boundaries of each group, proportion,
#'  adjusted proportion, mean explanatory variable, and (adjusted or unadjusted) log odds.
#'
#' @examples
#' data(MedGPA)
#' emplogitplot2(Acceptance~GPA+Sex,data=MedGPA)
#'
#' GroupTable2=emplogitplot2(Acceptance~MCAT+Sex,ngroups=5,out=TRUE,data=MedGPA,putlegend="topleft")
#'
#' emplogitplot2(Acceptance~MCAT+Sex,data=MedGPA,breaks=c(0,34.5,39.5,50.5),
#'               levelcol=c("red","blue"),putlegend="bottomright")

#' @export


emplogitplot2=function(formula,data=NULL,ngroups=3,breaks=NULL,
                       yes=NULL,padj=TRUE,out=FALSE,showplot=TRUE,showline=TRUE,
                       ylab="Log(Odds)",xlab=NULL,putlegend="n",
                       levelcol=NULL,pch=NULL,main="",
                       ylim=NULL,xlim=NULL,lty=NULL,lwd=1,cex=1){
  Factor=NULL; Z=NULL   #define these to pass package checks
  mod=glm(formula,family=binomial,data)
  newdata=mod$model[,1:3]
  oldnames=names(newdata)
  if(is.null(xlab)){xlab=oldnames[2]}   #Need a label for x-axis
  names(newdata)=c("Y","X","Z")
  newdata=na.omit(newdata)      #get rid of NA cases for either variable
  #if needed find the value for "success"
  newdata$Y=factor(newdata$Y)
  if(is.null(yes)){yes=levels(newdata$Y)[2]}
  if(ngroups=="all"){breaks=unique(sort(c(newdata$X,min(newdata$X)-1)))}
  if(is.null(breaks)){
    breaks= quantile(newdata$X, probs = (0:ngroups)/ngroups)
    breaks[1] <- breaks[1]-1
  }
  ngroups=length(breaks)-1
  Zlevels=levels(newdata$Z)
  numlevels=length(Zlevels)
  FactorData=NULL
  for (i in Zlevels){
    NewLevel=cbind(Factor=i,emplogitplot1(Y~X,breaks=breaks,data=subset(newdata,Z==i),padj=padj,out=TRUE,showplot=FALSE))
    FactorData=rbind(FactorData,NewLevel)
  }
  #Set up plot symbols if needed
  if(is.null(levelcol))   {levelcol =1:numlevels}
  if(is.null(pch))      {pch    =16+(1:numlevels)}
  if(is.null(lty))      {lty    =1:numlevels}
  #Make the plot
  if(showplot){
    plot(Logit~XMean,data=FactorData,type="n",ylab=ylab,ylim=ylim,xlim=xlim,xlab=xlab,cex=cex,main=main)
    for (i in 1:numlevels){
      Sub=subset(FactorData,Factor==Zlevels[i])
      points(Sub$XMean,Sub$Logit,pch=pch[i],col=levelcol[i],cex=cex)
      if(showline){abline(lm(Logit~XMean,data=Sub),col=levelcol[i],lty=lty[i],lwd=lwd)}
    }
    if(putlegend!="n"){   #add a legend
      legend(putlegend,legend=Zlevels,col=levelcol,lty=lty,title=oldnames[3])
      }
    }

  if(out){return(FactorData)}
}
