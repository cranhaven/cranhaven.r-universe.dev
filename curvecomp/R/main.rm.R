#' @title Parameteric Bootstrap Multiple Curve Comparisons for Repeated Measures
#'
#' @description Resampling-based multiple comparisons of adjusted p-values for curve observations with repeated measures.
#' @export
#' @param data A matrix specifying data. Each row in the data represents all observations from a specific subject. It is assumed that the same number of observations are stored for each time point.
#' @param treatment An integer specifying the number of treatments used for the dataset. \code{treatment} gives the number of time points (treatments) used for the dataset.
#' @param nboot An integer specifying the number of bootstraps. The default option is 1000. Note that, depending on the number of bootstraps, it may take a little while to compute the results.
#' @param type A character string specifying the contrast matrix type corresponding to the ones in \code{contrMat()} in the multcomp package.
#' @param Cmat A matrix specifying the contrast matrix. Useful if it is none of the ones in \code{contrMat()}. Either `type' or `Cmat' must be specified. The default option is NULL.
#' @param comparison.names A character vector supplying names for the comparisons implied in the contrast matrix. The default option is NULL, in which case, they are automatically generated.
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
#' \item{es.mat}{A matrix specifying all the effect sizes for each comparison at each time point. \code{es.mat} is simply \code{test.statistic.all} divided by the square root of the number of subjects (analogous to Cohen's d). Each row corresponds to each comparison, and each column corresponds to each time point.}
#' @import multcomp
#' @import stats
#' @import graphics
#' @import grDevices
#'
#' @examples
#'  ### Example with 10 subjects and 3 treatments having 100 observations per treatment.
#'  #nboot>=1000 is strongly recommended but we use nboot=5 here
#'  #to cut back on run time.
#'  nboot<-5
#'  n<-10
#'  treatment<-3
#'  m<-120
#'  range<-c(11:110)
#'  means<-c(0,0.003,0.006)
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
#'  for(i in 1:treatment)
#'  {
#'    mean<-means[i]
#'    sd<-sds[i]
#'    datamat<-matrix(rnorm((n*m),mean,sd),ncol=m)
#'    smoothdata<-t(apply(datamat,1,smoother))
#'    data<-cbind(data,smoothdata[,range])
#'  }
#'
#'  #Visualizing the data having three treatments
#'  plot(data[1,]~c(1:300),type='n',main="Simulated Data",
#'       ylim=c(min(data),max(data)),xlab="Observation",ylab="Data")
#'  for(j in 1:n)
#'  {
#'    lines(data[j,c(1:100)]~c(1:100),col=j)
#'    lines(data[j,c(101:200)]~c(101:200),col=j)
#'    lines(data[j,c(201:300)]~c(201:300),col=j)
#'  }
#'  abline(v=100,lty=2)
#'  abline(v=200,lty=2)
#'
#'  #Running the test
#'  #May take a little while to run.
#'
#'  testresults<-param.boot.rm(data=data,treatment=treatment,nboot=nboot,
#'  type="Tukey",fix.seed=TRUE,description=" (test_rm)")
#'  testresults
#'
#'


param.boot.rm<-function(data,treatment,nboot=1000,type,Cmat=NULL,comparison.names=NULL,plot.results=TRUE,conf.level=0.95,
xlab="",adj=1.5,rounds=3,start=0.05,end=0.8,alphas=c(0.01,0.05,0.10),fix.seed=FALSE,description="")
{
 #library(multcomp, quietly=TRUE)

 n<-dim(data)[1] ### number of subjects
 d<-dim(data)[2] ### total number of repeated trials per subject
 if(d%%treatment!=0)
 {
  stop("Number of observations for each time point is unequal.")
 }

 ### Number of observations per treatment.
 obs.per.time<-d/treatment
 time<-rep(c(1:treatment), each=obs.per.time)

 ### locations of the NA entries are stored.
 namat<-which(is.na(data)==1,arr.ind=TRUE)

 ### This function is modified for the repeated measures case.
 tstat.mat.calc.repeated<-function(data,time,type)
 {
  time.size<-as.vector(table(time))
  if(is.null(Cmat))
  {
   C<-contrMat(time.size,type=type)
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
  nc<-dim(C)[1]  ### number of contrasts, which is equal to the number of rows for C.
  connames <- rownames(C) ### name for each contrast.
  group <- rep(c(1:nc), each=n) ### data group for each contrast.

  meanvec<-colMeans(data,na.rm=TRUE)
  if(sum(is.na(meanvec)) > 0) stop("Too many NAs.")

  ### modifying the shape of the mean vector.
  ### matrix for each time point is stacked one after another.
  transmean <- matrix(meanvec,ncol=obs.per.time,byrow=TRUE)

  ### contrast is calculated for each time point.
  stat.mat<-c()
  mean.diff.mat<-c()
  var.diff.mat<-c()
  effect.size.mat<-c()

  numerators<-C%*%transmean
  denominators<-matrix(NA,nrow=nc,ncol=obs.per.time)

  for(k in 1:obs.per.time)
  {
   kcols<-c(1:treatment)*obs.per.time-obs.per.time+k
   datak<-data[,kcols]
   covmatk<-cov(datak, use="pairwise.complete.obs")/n
   if(sum(is.na(covmatk)) > 0) stop("Too many NAs.")
   for(j in 1:nc)
   {
   	Cj<-matrix(C[j,],nrow=1)
   	sck<-svd(covmatk)
   	rtsck<-Cj%*%sck$u%*%diag(sqrt(sck$d))%*%t(sck$v)
   	denominators[j,k]<-sqrt(rtsck%*%t(rtsck))
   }
  }

  tstat.mat<-numerators/denominators
  effect.size.mat<-tstat.mat/sqrt(n)
  stat.mat<-abs(tstat.mat)
  mean.diff.mat<-numerators
  var.diff.mat<-denominators^2

  rownames(stat.mat)<-connames
  rownames(mean.diff.mat)<-connames
  rownames(var.diff.mat)<-connames
  rownames(effect.size.mat)<-connames

  return(list(stat.mat=stat.mat, mean.diff.mat=mean.diff.mat, var.diff.mat=var.diff.mat,
  effect.size.mat=effect.size.mat, connames=connames))
 }

 tmat.info<-tstat.mat.calc.repeated(data=data,time=time,type=type)
 tmat<-tmat.info$stat.mat
 connames<-tmat.info$connames

### bootstrap step
 cat("It may take a little while to calculate the bootstrap results.","\n")
 if(plot.results==TRUE)
 {
  cat("Also, note that the plots of the results are saved as .pdf in the current directory.","\n")
 }
 vdata<-var(data, use="pairwise.complete.obs")
 sigsvd<-svd(vdata)
 retval1<-t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
 if(fix.seed==TRUE)
 {
  set.seed(1)
 }
 xboot<-matrix(rnorm(n*nboot*d),nrow=n*nboot,ncol=d)
 xpar<-xboot%*%retval1

 p.value.all<-matrix(0,nrow=(dim(tmat)[1]),ncol=(dim(tmat)[2]))
 rownames(p.value.all)<-connames

 ### a vector storing all the extreme t-statistics (only 't' option considered. no 'p' option.)
 extremevec<-double(nboot)
 mean.diff.mat<-NULL
 lower.diff.mat<-NULL
 upper.diff.mat<-NULL

 for(i in 1:nboot)
 {
  first<-n*(i-1)+1
  last<-n*i
  simdata<-xpar[c(first:last),]
  simdata[namat]<-NA
  simtmat<-tstat.mat.calc.repeated(data=simdata,time=time,type=type)$stat.mat
  simtextreme<-max(c(simtmat))
  p.value.all<-p.value.all + (tmat <= simtextreme)
  extremevec[i]<-simtextreme
 }

 extremevec<-sort(extremevec, decreasing=TRUE)
 p.value.all<-p.value.all/nboot
 p.value.comps<-apply(p.value.all,1,min)
 p.value.overall<-min(p.value.comps)

 test.statistic.comps<-apply(tmat,1,max)
 test.statistic.overall<-max(test.statistic.comps)

 ### For the testing purposes plot.results==FALSE is preferred.
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

  se.diff.mat<-alpha*sqrt(tmat.info$var.diff.mat)
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
  legendnames<-paste(connames[1], ", p=", sprintf(paste("%.", rounds, "f",
  sep=""), p.value.comps[1]), sep="")
  if(nr > 1)
  {
   for(i in 2:nr)
   {
    legendnames<-c(legendnames, paste(connames[i], ", p=", sprintf(paste("%.",
    rounds, "f", sep=""), p.value.comps[i]), sep=""))
   }
  }

  legend("topright",legendnames, col=color, lwd=2)
  dev.off()

  ### adjusted p-value plot.
  pdf("Adjusted_p_values.pdf")
  plot(p.value.all[1,], type="n", lwd=2, ylim=c(0,adj), ylab="Adjusted p-value", xlab=xlab)
  title(main=paste("Adjusted p-values", description, sep=""))
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

 #saving the results in a .txt file
 #resultmat<-rbind(p.value.all,tmat,mean.diff.mat,upper.diff.mat,lower.diff.mat,es.mat)
 #write.table(resultmat,file=paste("Results",description,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
 #write.table(connames,file=paste("Auxilirary Info",description,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)

 ### returns the results.
 return(list(p.value.all=p.value.all,p.value.comps=p.value.comps,p.value.overall=p.value.overall,
test.statistic.all=tmat,test.statistic.comps=test.statistic.comps,test.statistic.overall=test.statistic.overall,
 mean.diff.mat=mean.diff.mat, upper.diff.mat=upper.diff.mat, lower.diff.mat=lower.diff.mat,es.mat=es.mat))
}
