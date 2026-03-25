#' @title Parameteric Bootstrap Multiple Curve Comparisons for Independent Groups
#'
#' @description Resampling-based multiple comparisons of adjusted p-values for curve observations with independent groups.
#' @export
#' @param data A matrix specifying data. Each row in the data represents all observations from a specific subject. The number of rows in the data must be the same as the length of the group vector.
#' @param group A vector specifying which treatment group each subject belongs to. Must have the same length as the number of rows in data.
#' @param nboot An integer specifying the number of bootstraps. The default option is 1000.
#' @param type A character string specifying the contrast matrix type corresponding to the ones in \code{contrMat()} in the multcomp package.
#' @param Cmat A matrix specifying the contrast matrix. Useful if it is none of the ones in \code{contrMat()}. Either `type' or `Cmat' must be specified. The default option is NULL.
#' @param comparison.names A character vector supplying names for the comparisons implied in the contrast matrix.
#' @param option A character specifying whether MinP or MaxT should be used for the adjusted p-value calculation. "p" for MinP and "t" for MaxT.
#' @param plot.results A boolean specifying whether or not the visuals for the results should be created. The default option is TRUE. All the visuals (simultaneous confidence bands, adjusted p-values, and effect size plots) will be saved in the current directory.
#' @param conf.level A numeric that specifies the confidence level of the simultaneous confidence bands. It is one minus the familywise error rate. The default option is 0.95.
#' @param xlab A character string specifying the name to be used for the x-axis label of the plots. The default option is the empty character string.
#' @param adj A numeric specifying the adjustment factor for the plots. By increasing the number, more white spaces will be created in the plots. The default option is 1.5.
#' @param rounds An integer specifying the number of decimal places to be used. The default option is 3.
#' @param start A numeric specifying the greyscale to be used for the plot of the simultaneous confidence bands. The value must be greater than zero and less than the value specified for 'end'. The default option is 0.05.
#' @param end A numeric specifying the greyscale to be used for the plot of the simultaneous confidence bands. The value must be greater than the value specified for 'start' and less than one. The default option is 0.8.
#' @param alphas A numeric vector specifying the FWER cut-off values to be displayed in the plot of the adjusted p-values. The default option is c(0.01,0.05,0.10).
#' @param fix.seed A boolean specifying whether or not the seed for the random number generation should be set. This option is useful from the reproducibility point of view. The default option is FALSE.
#' @param description A character string specifying the experiment. The default option is the empty character string.
#'
#' @return 
#' \item{p.value.all}{A matrix specifying all the adjusted p-values for each comparison at each time point. Each row corresponds to each comparison, and each column corresponds to each time point.}
#' \item{p.value.comps}{A numeric vector specifying all the adjusted p-values for each comparison. This is the same as the minimum adjusted p-values from each row in \code{p.value.all}.}
#' \item{p.value.overall}{A numeric specifying the adjusted p-value for the overall conclusion. This is the same as the minimum adjusted p-value in \code{p.value.comps}.}
#' \item{test.statistic.all}{A matrix specifying all the t-test statistics for each comparison at each time point. Each row corresponds to each comparison, and each column corresponds to each time point.}
#' \item{test.statistic.comps}{A numeric vector specifying all the test statistics for each comparison. This is the same as the maximum test statistics from each row in \code{test.statistic.all}.}
#' \item{test.statistic.overall}{A numeric specifying the adjusted p-value for the overall conclusion. This is the same as the maximum test statistic in \code{test.statistic.comps}.}
#' \item{mean.diff.mat}{A matrix specifying all the mean differences for each comparison at each time point. Each row corresponds to each comparison, and each column corresponds to each time point.}
#' \item{upper.diff.mat}{A matrix specifying all the upper bounds of the simultaneous confidence intervals for the mean differences for each comparison at each time point. Each row corresponds to each comparison, and each column corresponds to each time point.}
#' \item{lower.diff.mat}{A matrix specifying all the lower bounds of the simultaneous confidence intervals for the mean differences for each comparison at each time point. Each row corresponds to each comparison, and each column corresponds to each time point.}
#' \item{es.mat}{A matrix specifying all the effect sizes for each comparison at each time point. \code{es.mat} is the mean difference divided by the pooled standard deviation (Cohen's d). Each row corresponds to each comparison, and each column corresponds to each time point.}
#' @import multcomp
#' @import stats
#' @import grDevices
#' @import graphics
#'
#' @examples
#'  #Example with 12 subjects and 3 treatments groups (n1=3, n2=4, n3=5) 
#'  #having 300 observations per subject.
#'  #nboot>=1000 is strongly recommended but we use nboot=5 here
#'  #to cut back on run time.
#'  nboot<-5
#'  group<-c(rep(1,3),rep(2,4),rep(3,5))
#'  group.size<-as.vector(table(group))
#'  n<-length(group)
#'  k<-length(group.size)
#'  m<-360
#'  range<-c(31:330)
#'  means<-c(0,0,0)
#'  sds<-c(0.01,0.015,0.02)
#'
#'  smoother<-function(data, spar=0.95)
#'  {
#'    return(smooth.spline(x=data, spar=spar)$y)
#'  }
#'
#'  data<-c()
#'
#'  set.seed(1)
#'  for(i in 1:k)
#'  {
#'    group.num<-group.size[i]
#'    mean<-means[i]
#'    sd<-sds[i]
#'    datamat<-matrix(rnorm((group.num*m),mean,sd),ncol=m)
#'    smoothdata<-t(apply(datamat,1,smoother))
#'    data<-rbind(data,smoothdata[,range])
#'  }
#'
#'  #Visualizing the data having three treatment groups
#'  plot(data[1,]~c(1:300),type='n',main="Simulated Data",
#'       ylim=c(min(data),max(data)),xlab="Observation",ylab="Data")
#'  for(j in 1:n)
#'  {
#'    lines(data[j,]~c(1:300),col=group[j])
#'  }
#'
#'  #Running the test
#'  #May take a little while to run.
#'  testresults<-param.boot(data=data,group=group,nboot=nboot,type="Tukey",
#'  fix.seed=TRUE,description=" (test_ind)")
#'  testresults
#'
#'



param.boot<-function(data,group,nboot=1000,type,Cmat=NULL,comparison.names=NULL,option="p",plot.results=TRUE,
conf.level=0.95,xlab="",adj=1.5,rounds=3,start=0.05,end=0.8,alphas=c(0.01,0.05,0.10),fix.seed=FALSE,description="")
{
 #library(multcomp, quietly=TRUE)
 #library(grDevices, quietly=TRUE)

 ### getting the dimension of the data matrix/frame.
 n<-dim(data)[1]
 d<-dim(data)[2]

 ### locations of the NA entries are stored.
 namat<-which(is.na(data)==1,arr.ind=TRUE)

 group.orig<-group
 group.label<-unique(group)
 group<-match(group.orig,group.label)

 ### tstat.mat.calc is a helper function for calculating the test statitics and p-values at each time point and contrast.
 tstat.mat.calc<-function(data,group,type,Cmat,option=c("t","p"))
 {
  option<-match.arg(option)
  mean.fun<-function(vector,group) return(tapply(vector,group,mean, na.rm=TRUE))
  var.fun<-function(vector,group) return(tapply(vector,group,var, na.rm=TRUE))
  mean.mat<-apply(data,2,mean.fun,group=group)
  var.mat<-apply(data,2,var.fun,group=group)
  group.size<-as.vector(table(group))
  inv.group.mat<-diag(1/group.size)
  nsp2.mat<-matrix((group.size-1)/(sum(group.size-1)),nrow=1)
  sp2.vec<-nsp2.mat%*%var.mat
  if(is.null(Cmat))
  {
   C<-contrMat(group.size,type=type)
  }
  else
  {
   C<-Cmat
   if(is.null(rownames(C)))
   {
    rownames(C)<-1:(dim(Cmat)[1])
   }
  }
  connames <- rownames(C)
  if(!is.null(comparison.names))
  {
   connames <- comparison.names
   rownames(C) <- connames
  }
  mean.diff.mat<-C%*%mean.mat
  sp2.mat<-matrix(rep(sp2.vec,nrow(C)),byrow=TRUE,nrow=nrow(C))
  effect.size.mat<-mean.diff.mat/sqrt(sp2.mat)
  C2<-C^2
  var.diff.mat<-C2%*%inv.group.mat%*%var.mat
  tstat.mat<-mean.diff.mat/sqrt(var.diff.mat)
  abs.tstat.mat<-abs(tstat.mat)
  stat.mat<-abs.tstat.mat
  df.mat<-NULL
  if(option=="p")
  {
   df.top.mat<-var.diff.mat^2
   C4<-C^4
   inv.group.mat2<-diag(1/(group.size^2*(group.size-1)))
   var.mat2<-var.mat^2
   df.bottom.mat<-C4%*%inv.group.mat2%*%var.mat2
   df.mat<-df.top.mat/df.bottom.mat

   ### tstatdf is a vector of length 2.
   p.value.fun<-function(tstatdf) return(2*pt(q=tstatdf[1], df=tstatdf[2], lower.tail=FALSE))
   abs.tstat.vec<-c(abs.tstat.mat)
   df.vec<-c(df.mat)
   tstatdf.mat<-rbind(abs.tstat.vec,df.vec)
   p.value.vec<-apply(tstatdf.mat,2,p.value.fun)
   p.value.mat<-matrix(p.value.vec,ncol=d)
   stat.mat<-p.value.mat
  }
  return(list(stat.mat=stat.mat, mean.diff.mat=mean.diff.mat, var.diff.mat=var.diff.mat,
  df.mat=df.mat, effect.size.mat=effect.size.mat, connames=connames))
 }

 ### tmat stores information about the test statistics/p-values.
 tmat.info<-tstat.mat.calc(data=data,group=group,type=type,Cmat=Cmat,option=option)
 tmat<-tmat.info$stat.mat
 connames<-tmat.info$connames

 xpar<-matrix(0,nrow=n*nboot,ncol=d)

 ### bootstrap step
 cat("It may take a little while to calculate the bootstrap results.","\n")
 if(plot.results==TRUE)
 {
  cat("Also, note that the plots of the results are saved as .pdf in the current directory.","\n")
 }

 for(i in 1:max(group))
 {
  group.which<-which(group==i)
  group.data<-data[group.which,]
  group.n<-length(group.which)
  group.vdata<-var(group.data, use="pairwise.complete.obs")
  sigsvd<-svd(group.vdata)
  retval1<-t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
  if(fix.seed==TRUE)
  {
   set.seed(i)
  }
  group.xboot<-matrix(rnorm(group.n*nboot*d),nrow=group.n*nboot,ncol=d)
  group.xpar<-group.xboot%*%retval1
  group.index<-rep((0:(nboot-1))*n,each=group.n)+rep(group.which,nboot)
  xpar[group.index,]<-group.xpar
 }

 p.value.all<-matrix(0,nrow=(dim(tmat)[1]),ncol=(dim(tmat)[2]))
 rownames(p.value.all)<-connames

 ### a vector storing all the extreme t-statistics or p-values.
 extremevec<-double(nboot)

 ### adjusted p-value calculations.
 mean.diff.mat<-NULL
 lower.diff.mat<-NULL
 upper.diff.mat<-NULL
 for(i in 1:nboot)
 {
  first<-n*(i-1)+1
  last<-n*i
  simdata<-xpar[c(first:last),]
  simdata[namat]<-NA
  simtmat<-tstat.mat.calc(data=simdata,group=group,type=type,Cmat=Cmat,option=option)$stat.mat
  if(option=="t")
  {
   simtextreme<-max(c(simtmat))
   p.value.all<-p.value.all + (tmat <= simtextreme)
  }
  else #if(option=="p")
  {
   simtextreme<-min(c(simtmat))
   p.value.all<-p.value.all + (tmat >= simtextreme)
  }
  extremevec[i]<-simtextreme
 }

 if(option=="t")
 {
  extremevec<-sort(extremevec, decreasing=TRUE)
 }
 else #if(option=="p")
 {
  extremevec<-sort(extremevec, decreasing=FALSE)
 }

 p.value.all<-p.value.all/nboot
 p.value.comps<-apply(p.value.all,1,min)
 p.value.overall<-min(p.value.comps)

 if(option=="t")
 {
  test.statistic.comps<-apply(tmat,1,max)
  test.statistic.overall<-max(test.statistic.comps)
 }
 else #if(option=="p")
 {
  test.statistic.comps<-apply(tmat,1,min)
  test.statistic.overall<-min(test.statistic.comps)
 }

 ### simultaneous confidence band calculation.
 ### also generates visual outputs.
 if(plot.results==TRUE)
 {
  location<-max((1-conf.level)*nboot,1)
  location1<-max(floor(location),1)
  location2<-ceiling(location)
  if(location1 < location2)
  {
   alpha<-extremevec[location1]*(location-location1)+extremevec[location2]*(location2-location)
  }
  else # location1==location2
  {
   alpha<-extremevec[location]
  }

  if(option=="t")
  {
   se.diff.mat<-alpha*sqrt(tmat.info$var.diff.mat)
  }
  else #if(option=="p")
  {
   se.diff.mat<-qt(alpha/2, tmat.info$df.mat, lower.tail=FALSE)*sqrt(tmat.info$var.diff.mat)
  }

  mean.diff.mat<-tmat.info$mean.diff.mat
  lower.diff.mat<-mean.diff.mat-se.diff.mat
  upper.diff.mat<-mean.diff.mat+se.diff.mat

  upper.plot.lim<-max(c(upper.diff.mat))
  lower.plot.lim<-min(c(lower.diff.mat))
  mid.plot.lim<-(lower.plot.lim+upper.plot.lim)/2
  real.upper.plot.lim<-mid.plot.lim+(upper.plot.lim-mid.plot.lim)*adj

  pdf("Mean_Difference_Curves.pdf")
  plot(mean.diff.mat[1,], type="n", lwd=2, ylim=c(lower.plot.lim,real.upper.plot.lim), ylab="Difference", xlab=xlab)
  title(main=c(paste("Mean Difference Curves", description, sep=""), paste(100*conf.level, "% Simultaneous Confidence Bands", sep=""),
  paste("Overall p-value: ", sprintf(paste("%.", rounds, "f", sep=""), p.value.overall), sep="")))
  abline(h=0)

  nr<-nrow(mean.diff.mat)
  color<-gray.colors(nr,start=start,end=end)

  lines(mean.diff.mat[1,], col=color[1], lwd=2)
  lines(upper.diff.mat[1,], col=color[1], lwd=2, lty=2)
  lines(lower.diff.mat[1,], col=color[1], lwd=2, lty=2)
  nr<-nrow(mean.diff.mat)
  if(nr > 1)
  {
   for(i in 2:nr)
   {
    lines(mean.diff.mat[i,], col=color[i], lwd=2)
    lines(upper.diff.mat[i,], col=color[i], lwd=2, lty=2)
    lines(lower.diff.mat[i,], col=color[i], lwd=2, lty=2)
   }
  }
  par(xpd=TRUE)
  legendnames<-paste(connames[1], ", p=", sprintf(paste("%.", rounds, "f", sep=""), p.value.comps[1]), sep="")
  if(nr > 1)
  {
   for(i in 2:nr)
   {
    legendnames<-c(legendnames, paste(connames[i], ", p=", sprintf(paste("%.", rounds, "f", sep=""), p.value.comps[i]), sep=""))
   }
  }

  legend("topright",legendnames, col = color, lwd=2)
  dev.off()

  ### adjusted p-value plot.
  pdf("Adjusted_p_values.pdf")
  plot(p.value.all[1,], type="n", lwd=2, ylim=c(0,adj), ylab="Adjusted p-value", xlab=xlab)
  title(main=paste("Adjusted_p_values", description, sep=""))
  lines(p.value.all[1,], col=color[1], lwd=2)
  for(j in 1:length(alphas))
  {
   abline(h=alphas[j], lty=2)
  }
  if(nr > 1)
  {
   for(i in 2:nr)
   {
   	lines(p.value.all[i,], col=color[i], lwd=2)
   }
  }
  par(xpd=TRUE)
  legendnames.adjp<-paste(connames[1], ", p=", sprintf(paste("%.", rounds, "f", sep=""), p.value.comps[1]), sep="")
  if(nr > 1)
  {
   for(i in 2:nr)
   {
    legendnames.adjp<-c(legendnames.adjp, paste(connames[i], ", p=", sprintf(paste("%.", rounds, "f", sep=""), p.value.comps[i]), sep=""))
   }
  }

  legend("topright",legendnames.adjp, col = color, lwd=2)
  dev.off()

  ### effect size plot.
  pdf("Effect_Size_Curves.pdf")
  es.mat<-tmat.info$effect.size.mat
  upper.plot.es<-max(c(es.mat))
  lower.plot.es<-min(c(es.mat))
  mid.plot.es<-(lower.plot.es+upper.plot.es)/2
  real.upper.plot.es<-mid.plot.es+(upper.plot.es-mid.plot.es)*adj
  plot(es.mat[1,], type="n", lwd=2, ylim=c(lower.plot.es,real.upper.plot.es), ylab="Cohen's d", xlab=xlab)
  title(main=c(paste("Effect Size Curves", description, sep="")))
  abline(h=0)
  lines(es.mat[1,], col=color[1], lwd=2)
  if(nr > 1)
  {
   for(i in 2:nr)
   {
    lines(es.mat[i,], col=color[i], lwd=2)
   }
  }

  par(xpd=TRUE)
  legendnames.es<-paste(connames[1], sep="")
  if(nr > 1)
  {
   for(i in 2:nr)
   {
    legendnames.es<-c(legendnames.es, paste(connames[i], sep=""))
   }
  }
  legend("topright",legendnames.es, col = color, lwd=2)
  dev.off()
 }

 ### returns the results.
 return(list(p.value.all=p.value.all,p.value.comps=p.value.comps,p.value.overall=p.value.overall,
 test.statistic.all=tmat,test.statistic.comps=test.statistic.comps,test.statistic.overall=test.statistic.overall,
 mean.diff.mat=mean.diff.mat, upper.diff.mat=upper.diff.mat, lower.diff.mat=lower.diff.mat, es.mat=es.mat))
}



