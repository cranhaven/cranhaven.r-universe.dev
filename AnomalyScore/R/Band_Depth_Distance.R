#####  distance based on band depth for nonstationary time series   #####
## based on: Band Depth Clustering for Nonstationary Time Series and Wind Speed Behavior (2018) Tupper et al
## Adapted By Guillermo Granados
## Department of Mathematics and Statistics Lancaster University

## bands generation 
#' Pairwise band generation in a multivariate time series
#' 
#' takes all the pairs of series in a multivariate set and compute the band 
#' between each pair od series
#'
#' @param series A matrix of n columns representing a multivariate time series,
#'  each column is a univariate time series
#' @return a list with two elements the lowerbounds of all (n)(n-1)/2 pairs
#' and the upperbounds of the pairwise bands
#' @seealso Band Depth Clustering for Nonstationary Time Series and
#'  Wind Speed Behavior (2018) Tupper et al
#'  
#' @export
#' @examples
#' X=matrix( rnorm(200), ncol=10  )
#' all_bands(X)
#' 
all_bands=function(series){
  # computes all the bands   
  n=ncol(series)  
  Tlenght=nrow(series)
  # lower limits
  lower_bound=matrix(NA, ncol =Tlenght, nrow = n*(n-1)/2 )
  upper_bound=matrix(NA, ncol =Tlenght, nrow = n*(n-1)/2 )
  for(i in 1:(n-1)){
    for(j in (i+1):n ){
      tempmat=rbind(series[,i],series[,j])
      lower_bound[((j-i)+ (i-1)*n- i*(i-1)/2) ,  ]=apply(tempmat,2,  min  )
      upper_bound[((j-i)+ (i-1)*n- i*(i-1)/2) ,  ]=apply(tempmat,2,  max  )
    }
  }
  
  return(list(lower_bound=lower_bound,upper_bound=upper_bound ))
}

#' indexes where a series is within a specific band
#' 
#' Return the indicies in which the values of a series x are located within a 
#' band b, called the informative bands.  
#'
#' @param allbands a list with two elements the lowerbounds of all (n)(n-1)/2
#'  pairs and the upperbounds of the pairwise bands. 
#'  Result of the function all_bands 
#' @param x A vector  representing a univariate time series
#' @return A vector with indices
#' @seealso Band Depth Clustering for Nonstationary Time Series and
#' Wind Speed Behavior (2018) Tupper et al
#'  
#' @export
#' @examples
#' X=matrix( rnorm(200), ncol=10  )
#' M=all_bands(X)
#' informative_bands(M,X[,1] )
#' 
informative_bands=function( allbands, x ){
  #return the index of informative bands  
  n=nrow( allbands[[1]]  )
  Tbx=list()
  for(i in 1:n){
    Tbx[[i]]= which( allbands$lower_bound[i, ] <=x & x<=allbands$upper_bound[i, ] )  
  }
  return(Tbx)
}


#' Band depth distance between 2 time series given a set of bands
#' 
#' Distance based on a depth concept, given a set of bands a modified Jaccard 
#' measure is compute between the sets of indices that two series share,
#' the Jaccard distances then are averaged over all informative bands. 
#'
#' @param allbands a list with two elements the lowerbounds of all (n)(n-1)/2
#'  pairs and the upperbounds of the pairwise bands. 
#' @param x A vector  representing a univariate time series
#' @param y A vector  representing a univariate time series
#' @return A non-negative value representing the distance between two time 
#' series, based on the concept of band depth.
#' @seealso Band Depth Clustering for Nonstationary Time Series and
#'  Wind Speed Behavior (2018) Tupper et al
#'  
#' @export
#' @examples
#' X=matrix( rnorm(200), ncol=10  )
#' M=all_bands(X)
#' dxy_bands(M,X[,1],X[,2] )
#' 
dxy_bands=function(allbands,x, y ){
  tbx=  informative_bands( allbands, x )
  tby=  informative_bands( allbands, y )
  n=length(tbx)  
  dxy=0
  Bxy=0
  for(i in 1:n){
    union_tbx_tby=  union(tbx[[i]],tby[[i]] )
    if(length(union_tbx_tby) >0){
      dxy =dxy+1-length( intersect(tbx[[i]],tby[[i]]) )/ length(union_tbx_tby)
      Bxy=Bxy+1
    }
  }#end i
  Dxy=0
  if(Bxy>0){
    Dxy=dxy/Bxy
  }
  return(Dxy)  
}


