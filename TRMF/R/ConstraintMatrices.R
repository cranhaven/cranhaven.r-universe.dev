# A set of functions to create constraint matrices.

# Create a finite difference matrix, any order d including fractional and negative
FiniteDiffM = function(N,d){
  # Find finite difference coefficient
  u=1:(N-1)
  wt = rev(c(1,cumprod ((u-d-1)/u))) # generalized binomial series
  Dmat = Matrix::Matrix(0,N,N,sparse=TRUE)
  # shift through, create matrix
  for(rw in 1:N){
    Dmat[rw,1:rw] = wt[(N-rw+1):N]
  }
  
  # clear the first rows
  if(d>0){
    D = ceiling(d);
    Dmat[1:D,1:D] = 0
  }
  return(Dmat)
}

# AR matrix
ARmat = function(N,ar){
  
  # initial stuff
  mat = Matrix::Diagonal(N) # 1 - lagged...
  lngAR = length(ar)
  indset = (1:lngAR)-lngAR-1
  
  # Not possible in this case
  if(lngAR>=N){
    return(0*mat)
  }
  
  # cascade down
  for(rw in (lngAR+1):N){
    mat[rw,indset+rw] = -ar
  }
  return(mat)
}

# Create seasonal difference matrix
Seasonal_DM= function(N,lag=12,sumFirst=TRUE){
  Dm = Matrix::Diagonal(N)
  if( N <= lag){
    return(0*Dm)
  }
  Dm[row(Dm)==(col(Dm)+lag)]=-1
  Dm[1:lag,1:lag]=0
  if(sumFirst){
    Dm[1,1:lag] =1
  }
  return(Dm)
}

Seasonal_DM_t2= function(N,lag=12){
  Dm = Matrix::Diagonal(N)
  if( N <= lag){
    return(0*Dm)
  }
  
  for(k in 1:(lag-1)){
    Dm[(row(Dm)==(col(Dm)+k))]=1
  }
  
  Dm[1:(lag-1),1:(lag-1)]=0
  return(Dm/lag)
}