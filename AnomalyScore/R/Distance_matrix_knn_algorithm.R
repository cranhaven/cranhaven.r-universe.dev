#### Distance Matrices and knn algorithm for different distances####
## By Guillermo Granados
## Department of Mathematics and Statistics Lancaster University

#compute matrix of distances among devices TS
#' Distance matrix from a pattern recognition distance
#'
#' pairwise distance matrix of a multivariate time series based on a value 
#' (Euclidean distance) and behavior (temporal correlation) measures
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param k The parameter $k$ controls the contribution of the sum of squares 
#' comparison as a value-based metric and the $Cort$ quantity as a behavioral 
#' metric; when $k=0$, then the distance is equal to the value-based metric, 
#' on the other hand, when $k=6$ the distance is mainly determined by the value 
#' of the temporal correlation $Cort$.
#' @return a matrix with pairwise distances
#' @seealso Douzal-Chouakria, Ahlame, and Cecile Amblard. "Classification
#'  Trees for Time Series." Pattern Recognition 45, no. 3 (March 2012): 
#'  1076-91. \doi{10.1016/j.patcog.2011.08.018}
#'  
#' @export
#' @examples
#' X=matrix( rnorm(200), ncol=10  )
#' k=2
#' distance_matrix_cort(k,X)
distance_matrix_cort=function(k,unit){
  # unit is a TS columns are the devices,
  # rows number of points
  cols=ncol(unit)    
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 2:cols){
    for(j in 1:(i-1) ){
      dmat[i,j] = DEcort(k,unit[,i],unit[,j])  
    }
  }
  dmat=dmat+t(dmat)
  return( dmat)
}


#' Normalized distance matrix from a pattern recognition distance
#'
#' pairwise distance matrix of a multivariate time series based on a value 
#' (Coefficient of variation) and behavior (temporal correlation) measures
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param k The parameter $k$ controls the contribution of the sum of squares 
#' comparison as a value-based metric and the $Cort$ quantity as a behavioral 
#' metric; when $k=0$, then the distance is equal to the value-based metric, 
#' on the other hand, when $k=6$ the distance is mainly determined by the value 
#' of the temporal correlation $Cort$.
#' @return a matrix with pairwise distances
#' @seealso Guillermo Granados, and Idris Eckley. "Electricity Demand of Buildings Benchmarked via Regression Trees on Nearest Neighbors Anomaly Scores" 
#'  
#' @export
#' @examples
#' X=matrix( rnorm(200), ncol=10  )
#' k=2
#' distance_matrix_cortNorm(k,X)
distance_matrix_cortNorm=function(k,unit){
  # unit is a TS columns are the devices,
  # rows number of points
  cols=ncol(unit)    
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 2:cols){
    for(j in 1:(i-1) ){
      dmat[i,j] = DEcortNorm(k,unit[,i],unit[,j])  
    }
  }
  dmat=dmat+t(dmat)
  return( dmat)
}

#' Normalized distance matrix from dynamic time-warping distance
#'
#' pairwise distance matrix of a multivariate time series based on a minimal 
#' mapping between two time series weighted by temporal correlation
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param k The parameter $k$ controls the contribution of the sum of squares 
#' comparison as a value-based metric and the $Cort$ quantity as a behavioral 
#' metric; when $k=0$, then the distance is equal to the value-based metric, 
#' on the other hand, when $k=6$ the distance is mainly determined by the value 
#' of the temporal correlation $Cort$.
#' @param maxwindow the maximum shift allowed between time series points.
#' @return a matrix with pairwise distances
#' @seealso For more details, check the `dtw` package documentation on CRAN   
#'  
#' @export
#' @examples
#' X=matrix( rnorm(200), ncol=10  )
#' k=2
#' maxwindow=10
#' distance_matrix_dtw(k,X,maxwindow)
distance_matrix_dtw=function(k,unit,maxwindow){
  # unit is a TS columns are the devices,
  # rows number of points
  cols=ncol(unit)    
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 2:cols){
    for(j in 1:(i-1) ){
      x1=unit[ which(!is.na(unit[,i]) & !is.na(unit[,j])  ),i]
      x2=unit[ which(!is.na(unit[,i]) & !is.na(unit[,j])  ),j]
      dmat[i,j] = DTWcort(k,x1,x2,maxwindow)  
      
    }
  }
  dmat=dmat+t(dmat)
  return( dmat)
}

## distance matrix coherence
#' Distance matrix from a coherence measure
#'
#' Pairwise distance matrix of a multivariate time series based on computing the 
#' squared coherence and transformed it to represent a distance at a specific
#' frequency
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param span1 Odd integer giving the widths of modified Daniell 
#' smoothers to be used to smooth the periodogram. Refers to the bandwidth of
#'  the smoothing process.
#' @param span2 Odd integer giving the widths of modified Daniell 
#' smoothers to be used to smooth the periodogram. Control another level of 
#' smoothing to the spectral density estimation without altering the peaks
#' @param period Integer referencing the index of the frequency to use for the
#' distance. It gives the Hertz or periods per unit of time; i.e., if the 
#' sampling is per minute, and each hour cycle is the period of interest
#  period=(length of series)/60 
#' @return a matrix with pairwise distances
#' @seealso For more details, check the `astsa` package documentation on CRAN or visit
#' the GitHub repository \url{https://github.com/nickpoison/astsa}. 
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' span1=2
#' span2=2
#' period=3
#' distance_matrix_coherence(unit=X, span1, span2, period )
distance_matrix_coherence=function(  unit, span1, span2, period ){
  # span 1 refers to the bandwidth of the smoothing process
  # span2 control another level of smoothing to the curve not altering the peaks  
  # period refers to the number of time points it comprises a period of interest
  # i.e., if the sampling is per minute, and each hour cycle is the period of interest
  # period=(length of series)/60 
  spetest= astsa::mvspec(unit,span=rep(span1,span2), demean = T, detrend = F, 
                  plot = F, na.action = na.exclude )
  myfreq=round( period,0)# ensure the period is an integer
  dimunit=dim(unit)[2]
  distmat=matrix(0,nrow = dimunit,ncol = dimunit )
  for(j in 2:dimunit){
    for(i in 1:(j-1)){
      distmat[j,i]= 1.0000000000-spetest$coh[myfreq,(i+(j-1)*(j-2)/2 )  ]    
    } 
  }
  distmat=distmat+t(distmat)
  return(distmat)
}



#' Distance matrix from based on the Wasserstein distance
#'
#' Pairwise distance matrix of a multivariate time series based on the 
#' Wasserstein distance between the empirical distribution of the series
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @return a matrix with pairwise distances
#' @seealso For more details, check the `transport` package documentation on CRAN
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' distance_matrix_wasserstein(unit=X)
distance_matrix_wasserstein=function(  unit ){
  # unit is a TS columns are the devices,
  # rows number of points
  cols=ncol(unit)    
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 2:cols){
    for(j in 1:(i-1) ){
      x1=unit[ which(!is.na(unit[,i]) & !is.na(unit[,j]) ),i]
      x2=unit[ which(!is.na(unit[,i]) & !is.na(unit[,j]) ),j]
      dmat[i,j] = transport::wasserstein1d(x1,x2)  
      
    }
  }
  dmat=dmat+t(dmat)
  return( dmat)
}


#' Distance matrix from a partial directed coherence measure (PDC)
#'
#' Pairwise distance matrix of a multivariate time series based on the
#' partial directed coherence among two series. The distance considers both 
#' directions of causality and transform it to give 0 in absence of 
#' causality between the series. 
#' 
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param ar  Integer vector containing all the lags considered for the
#' vector autoregressive model
#' @param period Integer referencing the index of the frequency to use for the
#' distance. It gives the Hertz or periods per unit of time; i.e., if the 
#' sampling is per minute, and each hour cycle is the period of interest
#  period=(length of series)/60 
#' @return a matrix with pairwise distances
#' @seealso Guillermo Granados, and Idris Eckley. "Electricity Demand of Buildings Benchmarked via Regression Trees on Nearest Neighbors Anomaly Scores"
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' ar=c(1, 2)
#' period=10
#' distance_matrix_PDC(  unit=X, ar,  period )
distance_matrix_PDC=function(  unit, ar,  period ){
  #ar	autoregresssion definition.  
  # period refers to the number of time points it comprises a period of interest
  # i.e., if the sampling is per minute, and each hour cycle is the period of interest
  # period=(length of series)/60 
  PDC_mat= matrix_PDC(unit, ar )
  myfreq=round( period,0)# ensure the period is an integer
  
  A=abs(PDC_mat[,,myfreq] )
  A=(A+t(A))*.5
  os=matrix( 1.0000000000, nrow = nrow(A) , ncol =ncol(A))
  distmat=os-A
  diag(distmat)=0
  return(distmat)
}


##### distance matrix based on the mahalanobis distance ######
#' Pairwise distance matrix based on the mahalanobis distance
#'
#' Pairwise distance matrix of a multivariate time series based on the
#' Mahalanobis distance between two series, modified to consider the different 
#' scales of series
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @return a matrix with pairwise distances
#' @seealso Prekopcsak, Zoltan, and Daniel Lemire. "Time Series Classification
#' by Class-Specific Mahalanobis Distance Measures." Advances in Data Analysis
#' and Classification 6, no. 3 (October 2012): 185-200. \doi{10.1007/s11634-012-0110-6}
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' distance_matrix_mahalanobis(unit=X )
distance_matrix_mahalanobis=function(  unit ){
  sdrow=apply( unit,1,sd, na.rm=T )
  geommean= sqrt(exp( mean(log(sdrow))) )
  # unit is a TS columns are the devices,
  # rows number of points
  cols=ncol(unit)    
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 2:cols){
    for(j in 1:(i-1) ){
      dmat[i,j] = sqrt(sum(((unit[,i]-unit[,j])/sdrow)^2))*geommean
    }
  }
  dmat=dmat+t(dmat)
  return( dmat)
}



#####  Distance matrix based on the conditional Granger causality index CGCI  #####
#' Pairwise distance matrix based on the conditional Granger causality index
#'
#' Pairwise distance matrix of a multivariate time series based on the
#' the conditional Granger causality index distance between two series
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param pmax maximum order(lag) of the VAR model to be considered
#' @return a matrix with pairwise distances
#' @seealso Siggiridou, Elsa, and Dimitris Kugiumtzis. "Granger Causality 
#' in Multivariate Time Series Using a Time-Ordered Restricted Vector
#'  Autoregressive Model." IEEE Transactions on Signal Processing 64, no. 
#'  7 (April 2016): 1759-73. \doi{10.1109/TSP.2015.2500893}
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' pmax=4
#' distance_matrix_CGCI(unit=X, pmax)
distance_matrix_CGCI=function(unit, pmax){
  # unit is a TS columns are the devices,
  # pmax is maximum order to include in the mBTS algorithm
  cols=ncol(unit)    
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 1:cols){
    dmat[,i] = mBTSCGCI(xM=unit, responseindex=i, pmax=pmax)$RCGCIV 
  }
  
  dmat=(dmat+t(dmat) )*.5
  return( dmat)  
  
  
}




#### Distance matrix based on the restricted generalized partial directed coherence  #####
#' Pairwise distance matrix based on the restricted generalized partial
#' directed coherence
#'
#' Pairwise distance matrix of a multivariate time series based on the
#' the restricted generalized partial directed coherence distance 
#' between two series
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param pmax maximum order(lag) of the VAR model to be considered
#' @param period Integer referencing the index of the frequency to use for the
#' distance. It gives the Hertz or periods per unit of time; i.e., if the 
#' sampling is per minute, and each hour cycle is the period of interest
#  period=(length of series)/60 
#' @return a matrix with pairwise distances
#' @seealso Siggiridou, Elsa, Vasilios K. Kimiskidis, and Dimitris Kugiumtzis.
#' "Dimension Reduction of Frequency-Based Direct Granger Causality Measures
#' on Short Time Series." Journal of Neuroscience Methods 289 (September 2017)
#' : 64-74. \doi{10.1016/j.jneumeth.2017.06.021}

#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' pmax=4
#' period=3
#' distance_matrix_RGPDC(unit=X, pmax, period)
distance_matrix_RGPDC=function(unit, pmax, period){
  # unit is a TS columns are the devices,
  # pmax is maximum order to include in the mBTS algorithm
  cols=ncol(unit)  
  freqs=TSA::periodogram(unit[,1], plot=F)$freq
  darray<-mBTSRGPDC(xM=as.matrix(unit), pmax, freqs)  
  
  dmat=(darray[,,period]+t(darray[,,period]) )*.5
  os=matrix( 1.0000000000, nrow = cols , ncol =cols)
  distmat=os-dmat
  diag(distmat)=0
  return( distmat)  
  
}


#### Distance matrix based on the partial mutual information of mixed embedings method   ######
#' Pairwise distance matrix based on the partial mutual information of mixed 
#' embedings (PMIME) method
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @param Lmax : the maximum delay to search for X and Y components for the mixed 
#'           embedding vector ,default is 5.
#' @param Tl    : Tl steps ahead that the mixed embedding vector has to explain.
#'           Note that if Tl>1 the future vector is of length Tl and contains
#'           the samples at times t+1,..,t+Tl ,dafault is 1. 
#' @param nnei : number of nearest neighbors for density estimation ,default is 5
#' @param A    : the threshold for the ratio of CMI over MI of the lagged variables
#'           for the termination criterion.
#' @return a matrix with pairwise distances
#' 
#' @seealso Kugiumtzis, D. "Direct-Coupling Information Measure from Nonuniform
#' Embedding." Physical Review E 87, no. 6 (June 25, 2013): 062918. 
#' \doi{10.1103/PhysRevE.87.062918}
#'  
#' @export
#' @examples
#' X=matrix( rnorm(300), ncol=3  )
#' Lmax=2
#' Tl=1
#' nnei=5
#' A=.95
#' distance_matrix_PMIME(unit=X, Lmax, Tl, nnei, A )
distance_matrix_PMIME=function(unit, Lmax, Tl, nnei, A ){
  # unit: matrix containing times series in its colmns
  # Lmax: maximum lag order for the times series to run the PMIME  
  # Tl: future values of the response series to test in the PMIME method  
  # nnei: number nearest neighbors to compute the mutual information (MI) 
  PMIMEmat <- PMIME(allM=unit, Lmax = Lmax, Tl = Tl, nnei = nnei, A = A, showtxt = 0)$RM
  
  A=(PMIMEmat+t(PMIMEmat))*.5
  
  os=matrix( 1.0000000000, nrow = nrow(A) , ncol =ncol(A))
  distmat=os-A
  diag(distmat)=0
  return(distmat)
  
}

#### Distance matrix based on the multivariate locally wavelet partial coherence   ######
#' Pairwise distance matrix based on the multivariate locally wavelet partial 
#' coherence
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @return a matrix with pairwise distances
#' @seealso Park, Timothy, Idris A. Eckley, and Hernando C. Ombao. 
#' "Estimating Time-Evolving Partial Coherence Between Signals via Multivariate
#'  Locally Stationary Wavelet Processes." IEEE Transactions on Signal 
#'  Processing 62, no. 20 (October 2014): 5240-50. \doi{10.1109/TSP.2014.2343937}
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' distance_matrix_mvLWS(unit=X)
distance_matrix_mvLWS=function(unit ){
  tl=dim(unit)[1]
  k=round( log(tl)/log(2),0 )
  if(2^k>tl ){k=k-1 }
  unit2=unit[1:(2^k),]
  # unit: matrix containing times series in its colmns
  # the length of the time series should be of the form 2^J 
  mvwavelet= mvLSW::mvEWS(X=unit2, filter.number = 1, family = "DaubExPhase", 
                   smooth = TRUE, type = "all", optimize = T, 
                   smooth.Jset = NA, bias.correct = TRUE, tol = 1e-10,
                   verbose = FALSE)
  partialwave<- mvLSW::coherence(object = mvwavelet, partial = T)
  mattemp=-abs( partialwave$spectrum)
  distmat=   exp( apply( mattemp,c(1,2), mean  ) )
  
  diag(distmat)=0
  return(distmat)
}



# The distance matrix computes the band depth distance of Tupper et al (2018)
#for all combinations of series
#' Pairwise distance matrix based on the band depth distance 
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series. 
#' @return a matrix with pairwise distances
#' @seealso Band Depth Clustering for Nonstationary Time Series and
#'  Wind Speed Behavior (2018) Tupper et al
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' distance_matrix_banddepth(unit=X)
distance_matrix_banddepth=function(unit){
  # unit is a TS columns are the devices, and rows number of points
  cols=ncol(unit)    
  mybands=all_bands(unit)
  dmat<-matrix(0, ncol=cols, nrow=cols)  
  for(i in 2:cols){
    for(j in 1:(i-1) ){
      dmat[i,j] =dxy_bands(allbands=mybands,x=unit[,i], y=unit[,j] )  
    }
  }
  dmat=dmat+t(dmat)
  return( dmat)
}


# general knn algorithm
# dparams should be the parameters of the distance function, including the data
# knn is the number of neighbors to be considered as the nearest
# distance is a distance matrix functions  from the script Distanceamongtimeseriesfunctions.R
# The distance matrix computes the band depth distance of Tupper et al (2018)
#for all combinations of series
#' K-Nearest neighbors algorithm to compute an anomaly score
#' 
#' The method obtain a distance matrix and find the K-nearest neighbors of 
#' each series and sum their distances in the neighborhood.
#' The sum is defined as the anomaly score, the series with higher scores 
#' implies their neighbors are far away and such a series is a potential 
#' outlier
#'
#' @param knn number of nearest neighbors to consider for the anomaly score 
#' @param distance function name of the available distance matrices
#' @param dparams a list with all the parameters for the distance matrix
#' @return A list of two elements with the anomaly scores and the distance 
#' matrix  
#' @seealso Guillermo Granados, and Idris Eckley. "Electricity Demand of Buildings Benchmarked via Regression Trees on Nearest Neighbors Anomaly Scores"
#'  
#' @export
#' @examples
#' X=matrix( rnorm(2000), ncol=10  )
#' distance=distance_matrix_coherence
#' dparams=list(unit=X, span1=2, span2=2, period = 5 )
#' knn=5
#' kneighbors_distance_docall(knn,distance, dparams)
kneighbors_distance_docall=function(knn, distance, dparams ){
  dmat= do.call(distance,dparams)
  anomalyscore=c()
  for(ind in 1: ncol(dmat) ){
    ordered_distances=  order(dmat[,ind])
    kneigh_loc= ordered_distances[2:(knn+1)]  #location or nodes of the neighbors
    anomalyscore[ind]=mean(dmat[kneigh_loc,ind], na.rm = T )
  }
  return(list( anomalyscore=anomalyscore,dmat=dmat ) )  
}


# Anomaly score computation for a set of distances
# dparams should be the parameters of the distance function, including the data
# knn is the number of neighbors to be considered as the nearest
# measures is a vector indexing the distances to compute 
#' Anomaly score computation for a set of distances
#' 
#' Computes anomaly scores for a selection of different distances
#' for a single dataset.
#'
#' @param unit A matrix representing a multivariate time series where each 
#' column is a univariate time series.
#' @param knn number of nearest neighbors to consider for the anomaly scores 
#' @param measures vector with the indexes of the selected measures
#' 1=Cort, 2=Wasserstein, 3=Mahalanobis, 4=Normalized Cort,
#' 5=Coherence, 6=PDC, 7=CGCI,8=RGPDC, 9=PMIME, 10=mvLWS, 11=Band depth
#' @param dparams a list where each element is a list with all the parameters
#' necessary to compute the selected distances. If the distance does not need 
#' further parameters then define an empty list
#' @return A dataframe with the names of series in unit as a column called 
#' "series" and the corresponding scores computed for each distance. The rank is
#' ordered with respect to the first measure in the measures index vector 
#' @seealso Guillermo Granados, and Idris Eckley. "Electricity Demand of Buildings Benchmarked via Regression Trees on Nearest Neighbors Anomaly Scores"
#'  
#' @export
#' @examples
#' unit=matrix( rnorm(500), ncol=5  )
#' measures= c(1,5,11 ) # Cort, Coherence and Band depth
#' knn=3
#' dparams=list(
#'   list(k=2),
#'   list( span1=2, span2=2, period = 5),
#'   list( )
#' )
#' Anomalyscoresframe(unit, knn,measures, dparams)
Anomalyscoresframe=function(unit, knn,measures, dparams){
  dist_names<-c(
    'Cort',
    'WS',
    'MH',
    'CortN',
    'Coh',
    'PDC',
    'CGCI',
    'RGPDC',
    'PMIME',
    'mvLWS',
    'BD'
  )
  functions=list(
    distance_matrix_cort,
    distance_matrix_wasserstein,
    distance_matrix_mahalanobis,
    distance_matrix_cortNorm,
    distance_matrix_coherence,
    distance_matrix_PDC,
    distance_matrix_CGCI,
    distance_matrix_RGPDC,
    distance_matrix_PMIME,
    distance_matrix_mvLWS,
    distance_matrix_banddepth
  )
  unit=as.matrix(unit)
  myncols<-ncol(unit)
  if(is.null(colnames(unit)) ){
  unitnames<-paste0("Series_",1:myncols   )
  }else{
    unitnames=colnames(unit)
  }
  frame_scores= data.frame(series=unitnames)
  for(i in 1:length( measures) ){
    distance=functions[[measures[i] ]]
    mydparams=dparams[[ i ]]
    mydparams$unit=unit
    AScat1=kneighbors_distance_docall(knn, distance, mydparams )
    frame_scores$score= AScat1$anomalyscore
    names(frame_scores)[(i+1)]=dist_names[measures[ i]]
  }
  #rank is ordered with respect to the first measure
  frame_scores=frame_scores[order(frame_scores[,2], decreasing = T),]
  frame_scores$Rank= length( frame_scores$series):1
  return(frame_scores)
}


