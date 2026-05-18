
### Patern recognition distance functions
# By Guillermo Granados
# Department of Mathematics and Statistics Lancaster University

#' Temporal correlation coefficient 
#'
#' Return the temporal correlations of two time series by first taking the lag 
#' one differences of each series and the computing the correlation coefficient. 
#'
#' @param S1 A vector representing a univariate time series
#' @param S2 A second vector representing a univariate time series
#' @return A coefficient in the interval \eqn{[-1,1]} representing the lag 1 correlation
#' @seealso Douzal-Chouakria, Ahlame, and Cecile Amblard. "Classification
#'  Trees for Time Series." Pattern Recognition 45, no. 3 (March 2012): 
#'  1076-91. \doi{10.1016/j.patcog.2011.08.018}
#'  
#' @export
#' @examples
#' S1=rnorm(100)
#' S2=rnorm(100)
#' Cort(S1, S2)
Cort=function(S1, S2){
  insna=  which(is.na(S1)|is.na(S2)   )
  if(length(insna)>0){
    S1=S1[-insna]
    S2=S2[-insna]
  }
  #S1 S2 time series of the same size
  diffS1=diff(S1,lag=1)  
  diffS2=diff(S2,lag=1)  
  tempcor=sum( (diffS1 )*(diffS2), na.rm=T)/(sum(diffS1^2, na.rm=T)*sum(diffS2^2, na.rm=T) )^.5
  return(tempcor)
}


#' Distance based on value and behavior of the time series
#'
#' Return a weighted distance based on a weighted sum of the Euclidean norm 
#' and the temporal correlation coefficient. The distance is inflated in the 
#' presence of NA compensating for the lack of information. 
#'
#' @param S1 A vector representing a univariate time series
#' @param S2 A second vector representing a univariate time series
#' @param k The parameter $k$ controls the contribution of the sum of squares 
#' comparison as a value-based metric and the $Cort$ quantity as a behavioral 
#' metric; when $k=0$, then the distance is equal to the value-based metric, 
#' on the other hand, when $k=6$ the distance is mainly determined by the value 
#' of the temporal correlation $Cort$.
#' @return a non-zero value
#' @seealso Douzal-Chouakria, Ahlame, and Cecile Amblard. "Classification
#'  Trees for Time Series." Pattern Recognition 45, no. 3 (March 2012): 
#'  1076-91. \doi{10.1016/j.patcog.2011.08.018}
#'  
#' @export
#' @examples
#' S1=rnorm(100)
#' S2=rnorm(100)
#' k=1
#' DEcort(k,S1, S2)
DEcort=function(k, S1, S2){
  origlenn=length(S1)
  insna=  which(is.na(S1)|is.na(S2)   )
  if(length(insna)>0){
    S1=S1[-insna]
    S2=S2[-insna]
  }
  Euclidean_distance= sum(  (S1-S2)^2,na.rm=T )^.5 
  if(length(S1)>1 & length(S2)>1){
    if( var(S1,na.rm = T)==0|var(S2,na.rm = T)==0  ){
      dist= ( 1+ length(insna)/(origlenn- length(insna)))*Euclidean_distance
    }else{
      dist=  ( 1+ length(insna)/(origlenn- length(insna)) )*2*Euclidean_distance/(1+exp(k*Cort(S1,S2) ))
    }
  }else{ #length 0
    dist=Euclidean_distance
  }
  return(dist)
}

#' Normalized version of the Cort distance the modification is based on
#' using the coefficient of variation rather than euclidean distance, 
#' performed by normalizing by the absolute value of the total differences
#' of the series.
#'
#' @param S1 A vector representing a univariate time series
#' @param S2 A second vector representing a univariate time series
#' @param k The parameter $k$ controls the contribution of the sum of squares 
#' comparison as a value-based metric and the $Cort$ quantity as a behavioral 
#' metric; when $k=0$, then the distance is equal to the value-based metric, 
#' on the other hand, when $k=6$ the distance is mainly determined by the value 
#' of the temporal correlation $Cort$.
#' @return a non-zero value
#' @seealso Granados-Garcia, and Idris Eckley. "Building Electricity Demand 
#' Benchmarking via a Regression Trees on Anomaly Scores"
#'  
#' @export
#' @examples
#' S1=rnorm(100)
#' S2=rnorm(100)
#' k=1
#' DEcortNorm(k,S1, S2)
 
DEcortNorm=function(k, S1, S2){
  origlenn=length(S1)
  insna=  which(is.na(S1)|is.na(S2)   )
  if(length(insna)>0){
    S1=S1[-insna]
    S2=S2[-insna]
  }
  Euclidean_distance= sum(  (S1-S2)^2,na.rm=T )^.5 
  abs_dif=sum( abs(   (S1-S2) )  ,na.rm=T  )
  if(length(S1)>1 & length(S2)>1){
    if( var(S1,na.rm = T)==0|var(S2,na.rm = T)==0  ){
      dist= ( 1+ length(insna)/(origlenn- length(insna)))*Euclidean_distance/abs_dif
    }else{
      dist=  ( 1+ length(insna)/(origlenn- length(insna)) )*2*Euclidean_distance/( (1+exp(k*Cort(S1,S2) ))*(abs_dif) )
    }
  }else{ #length 0
    dist=Euclidean_distance
  }
  return(dist)
}


### summary :  apply dtw and add cort, minimizing over a choice of window values 
## maybe by a parmater max window, and find the minimum distance


#' Extention of the dynamic time warping distance
#' 
#' This function uses the dtw() function from the dtw R package to compute 
#' a distance based on the mapping than minimizes the distance between two sets 
#' of points, the parameters chosen are the "Manhattan" distance to compute the
#' differences between points and the "sakoechiba" window type. Important note: 
#' the dtw function does not accept NA values, therefore these types of values
#' are removed.
#'
#' @param S1 A vector representing a univariate time series
#' @param S2 A second vector representing a univariate time series
#' @param k The parameter $k$ controls the contribution of the sum of squares 
#' comparison as a value-based metric and the $Cort$ quantity as a behavioral 
#' metric; when $k=0$, then the distance is equal to the value-based metric, 
#' on the other hand, when $k=6$ the distance is mainly determined by the value 
#' of the temporal correlation $Cort$.
#' @param maxwindow the maximum shift allowed between time series points.
#' @return  A non-negative value representing the distance between two time 
#' series
#' @seealso Douzal-Chouakria, Ahlame, and Cecile Amblard. "Classification
#'  Trees for Time Series." Pattern Recognition 45, no. 3 (March 2012): 
#'  1076-91. \doi{10.1016/j.patcog.2011.08.018}
#'  
#' @export
#' @examples
#' S1=rnorm(100)
#' S2=rnorm(100)
#' k=1
#' maxwindow=20
#' DTWcort(k,S1, S2,maxwindow)
#' 
DTWcort=function(k, S1, S2,maxwindow){
  # the dtw function does not accept NA values 
  origlenn=length(S1)
  insna=  which(is.na(S1)|is.na(S2)   )
  #remove any NA dtw does not accept NA values 
  S1=na.omit( S1)
  S2=na.omit( S2)
  #normalization : natural peak of energy lead to misleading computations
  S1=(S1-mean(S1))/sd(S1)
  S2=(S2-mean(S2))/sd(S2)
  distprop=c()
  for(i in 1:maxwindow){
    dtwobject= dtw::dtw(S2,S1,dist.method ="Manhattan", window.type="sakoechiba", window.size=i, k=T)#
    if(length(S1)>1 & length(S2)>1){
      if( var(S1,na.rm = T)==0|var(S2,na.rm = T)==0  ){
        distprop[i]= ( 1+ length(insna)/(origlenn- length(insna)))*dtwobject$normalizedDistance
      }else{
        distprop[i]=  ( 1+ length(insna)/(origlenn- length(insna)) )*2*dtwobject$normalizedDistance/ (1+exp(k*Cort(S1,S2) )) 
      }
    }else{ #length 0
      distprop[i]=dtwobject$normalizedDistance
    }
    if(i>1){
      if( distprop[i]==distprop[(i-1)] ){break}
    }
  }
  dist=  min(distprop)
  return(dist)
}
