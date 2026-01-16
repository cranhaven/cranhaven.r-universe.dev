#' Stepp
#'
#' To compute and plot the observed and simulated distances for measuring similarity between time series.
#' The distance can be computed using ACF, PACF, AR-coefficients, or Periodogram.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param M Number of simulation realizations. Default value is 100.
#' @param lmax Number of lags used (for ACF, PACF, AR-coefficient). Default value is 5.
#' @param alpha Quantile used in the plotting. Default value is 0.95.
#' @param dismethod Summary statistics of each time series to be used in computing distance.
#' Choices include “ACF”, “PACF”, “AR.PIC” and “PER”. Default is "ACF".
#' @param clumethod Hierarchical clustering method:
#' choices include “single”, “average”, and “complete”. Default is “complete”.
#'
#' @details
#' The Empirical Dynamic Quantile of the series is obtained, a set of
#' Txk series is generated and the heights in the dendrogram are obtained.
#' This is repeated M times and the alpha quantile of the results of
#' the M simulations are reported. Both dendrogram's heights and steps
#' (differences) of these heights are compared.
#'
#' @return
#' Two plots are given in output:
#'
#' The first plot shows the “height” of the dendrogram. Solid line is the observed
#' height. The points denote the alpha quantile of heights based on the simulated series.
#'
#' The second plot shows the “step” of the dendrogam (increments of heights).
#' Solid line is the observed increments and the points are those of selected quantile
#' for the simulated series.
#'
#' A list containing:
#' \itemize{
#' \item mh - alpha quantile of heights based on the simulated series.
#' \item mdh - increments of selected quantile for the simulated series.
#' \item hgt - observed height.
#' \item hgtincre - observed increments.
#' \item Mh - the alpha quantile of the results of the M simulations are reported.
#' }
#'
#' @export
#'
#' @importFrom TSclust diss
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- stepp(as.matrix(TaiwanAirBox032017[,1:50]), M = 2)
#'
stepp <- function(x, M = 100, lmax = 5, alpha = .95, dismethod = "ACF",
                  clumethod = "complete"){

  if(!is.matrix(x))x <- as.matrix(x)
  T=dim(x)[1]
  k=dim(x)[2]
  if(dismethod=="AR.PIC"){
    pp <- rep(lmax,ncol(x))

    Macf <- TSclust::diss(t(x),METHOD="AR.PIC")
  }else{
    if(dismethod=="PER"){
      Macf <- TSclust::diss(t(x),METHOD="PER")
    }else{
      Macf <- TSclust::diss(t(x),METHOD =dismethod, lag.max=lmax)
    }
  }
  #
  Macf=as.matrix(Macf)
  sc1=hclust(as.dist(Macf),method=clumethod)
  wd1=diff(sc1$height)

  ost <- sample(1:k,1)
  mst=x[,ost]

  qq <- ar(mst)$order
  mqq <- arima(mst,order=c(qq,0,0))
  phi <- mqq$coef[1:qq]
  cnt <- mqq$coef[(qq+1)]
  sd <- sqrt(mqq$sigma2)

  Mh=matrix(0,k-1,M)
  Mdh=matrix(0,k-2,M)

  for (j in 1:M){

    xs=matrix(0,T,k)
    for (i in 1:k){
      y=arima.sim(n=T+50,model=list(ar=phi),sd=sd)
      y <- y+cnt
      xs[,i]=y[51:(T+50)]
    }
    if(dismethod=="AR.PIC"){
      Macf <- TSclust::diss(t(xs),METHOD="AR.PIC")
    }else{
      if(dismethod=="PER"){
        Macf <- TSclust::diss(t(xs),METHOD="PER")
      }else{
        Macf <- TSclust::diss(t(xs),METHOD =dismethod, lag.max=lmax)
      }
    }

    Macf=as.matrix(Macf)
    sc2=hclust(as.dist(Macf),method=clumethod)
    wd2=diff(sc2$height)
    Mdh[,j]=wd2
    Mh[,j]=sc2$height
  }
  mh=apply(Mh,1,quantile, probs=alpha)
  mdh=apply(Mdh,1,quantile, probs=alpha)

  ylim1 <- range(sc1$height,mh)
  plot(mh, ylim=ylim1*1.1, type="l", main ="Heights in Dendrogram", ylab="Heights")
  lines(sc1$height, type="p",cex=0.5)
  ylim2 <- range(wd1,mdh)
  plot(mdh, ylim=ylim2*1.1,type="l", main ="Height increments in Dendrogram", ylab="Height increment")
  lines(wd1, type="p",cex=0.5)

  return(list(mh = mh, mdh = mdh, hgt = sc1$height, hgtincre = wd1, Mh = Mh))
}
