#' Empirical logit plot for one quantitative variable
#'
#' @param formula A formula of the form (binary) Response~Predictor
#' @param data A dataframe
#' @param ngroups Number of groups to use (not needed if breaks is used), ngroups="all" uses all unique values
#' @param breaks A vector of endpoints for the bins (not needed if ngroups is used)
#' @param yes Set a value for the response to be counted for proportions (optional)
#' @param padj Should proportions be adjusted to avoid zero and one? (default is TRUE)
#' @param out Should the function return a dataframe with group information? (default is FALSE)
#' @param showplot Show the plot? default is TRUE
#' @param showline Show the regression line? default is TRUE
#' @param ylab Text label for the vertical axis (default is "Log(Odds)")
#' @param xlab Text label for the horizontal axis (default is NULL)
#' @param dotcol Color for the dots (default is "black")
#' @param linecol Color for the line (default is "black")
#' @param pch Plot character for the dots (default is 16)
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
#' This function produces an empirical logit plot for a binary response variable and a
#' single quantitative predictor variable.
#'
#' @details
#' Values of the quantitative explanatory variable will be grouped into \code{ngroups}
#' roughly equal sized groups, unless \code{breaks} is used to determine the boundaries of the groups.
#' Using \code{ngroups="all"} will make each distinct value of the explanatory variable its own group \cr
#'
#' We find an adjusted proportion for the binary response variable within each of the groups with
#' \code{(Number yes +0.5)/(Number of cases+1)}. This is converted to an adjusted log odds
#' \code{log(adjp/(1-adjp))}.  The adjustment avoids problems if there are no "successes" or
#' all "successes" in a group.  What constitutes a "success" can be specified with \code{yes=}
#' and the proportion adjustment can be turned off (if no group proportions are likely to be zero or one)
#' with \code{padj=FALSE}.\cr
#'
#' The function plots the log odds versus the mean of the explanatory variable within each group.
#'  A least square line is fit to these points.  The plot can be suppressed with \code{showplot=FALSE}.\cr
#'
#'  The \code{out=TRUE} option will return a dataframe with the boundaries of each group, proportion,
#'  adjusted proportion, mean explanatory variable, and (adjusted or unadjusted) log odds.
#'
#' @examples
#' data(MedGPA)
#' emplogitplot1(Acceptance~GPA,data=MedGPA)
#'
#' GroupTable=emplogitplot1(Acceptance~MCAT,ngroups=5,out=TRUE,data=MedGPA)
#'
#' emplogitplot1(Acceptance~MCAT,data=MedGPA,breaks=c(0,34.5,39.5,50.5),dotcol="red",linecol="black")
#'
#' data(Putts1)
#' emplogitplot1(Made~Length,data=Putts1,ngroups="all")
#'
#' @import graphics stats

#'
#' @export

emplogitplot1=function(formula,data=NULL,ngroups=3,breaks=NULL,
                       yes=NULL,padj=TRUE,out=FALSE,showplot=TRUE,showline=TRUE,
                       ylab="Log(Odds)",xlab=NULL,
                       dotcol="black",linecol="blue",pch=16,main="",
                       ylim=NULL,xlim=NULL,lty=1,lwd=1,cex=1){
  mod=glm(formula,family=binomial,data)
  newdata=mod$model[,1:2]
  oldnames=names(newdata)
  if(is.null(xlab)){xlab=oldnames[2]}   #Need a label for x-axis
  names(newdata)=c("Y","X")
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
  newdata$XGroups=cut(newdata$X,breaks=breaks,labels=1:ngroups)
  Cases=as.numeric(table(newdata$XGroups))
  XMean=as.numeric(aggregate(X~XGroups,data=newdata,mean)$X)
  XMin=as.numeric(aggregate(X~XGroups,data=newdata,min)$X)
  XMax=as.numeric(aggregate(X~XGroups,data=newdata,max)$X)
  NumYes=as.numeric(table(newdata$Y,newdata$XGroups)[yes,])
  Prop=round(NumYes/Cases,3)
  AdjProp=round((NumYes+0.5)/(Cases+1),3)
  Logit=as.numeric(log(AdjProp/(1-AdjProp)))
  if(!padj){Logit=as.numeric(log(Prop/(1-Prop)))}
  if(showplot){plot(Logit~XMean,ylab=ylab,col=dotcol,pch=pch,
       ylim=ylim,xlim=xlim,xlab=xlab,cex=cex,main=main)
  if(showline){abline(lm(Logit~XMean),col=linecol,lty=lty,lwd=lwd)}}
  GroupData=data.frame(Group=1:ngroups,Cases,XMin,XMax,XMean,NumYes,Prop,AdjProp,Logit)
  if(out){return(GroupData)}
}
