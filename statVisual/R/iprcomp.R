# v6 created on June 2, 2019
#  (1) replace missing value by median of corresponding variable. 
#  (Thank Mr. Jun Luo for pointing out the mistake in previous version:
#   replacing missing value by zero)
#
# v5 created on Feb. 17, 2019
#  (1) when number of variables are too large, the function 'eigen' will be
#      very slow. So we still use 'prcomp', but with modified input dat, 
#      in which NA is replaced by zero
# v4 created on Feb. 15, 2019
#  (1) use 'pairwise.complete.obs' instead of 'na.or.complete'
#      to handle the cases where all probes have at least one NA
#
# v3 created on Feb. 7, 2019
#  (1) add class to the output object so 'factoextra:::get_eig' can
#      handle the output
# v2 created on Jan. 29, 2019
#  (1) change '.scale' to 'scale.'
#  (2) simplify the R code
#
# created on Jan. 29, 2019
#  (1) calculate principal components when data containing missing values

# improved prcomp

# dat - n x p matrix; rows are subjects and columns are variables
iprcomp=function(dat, center=TRUE, scale. = FALSE)
{
  
  if(any(is.na(dat)==TRUE))
  {
    # impute missing values as the median of that variable
    dat2=apply(dat, 2, function(x){
      if(any(is.na(x)==TRUE)){
        m=median(x, na.rm=TRUE)
        pos.na=which(is.na(x)==TRUE)
        x[pos.na]=m
      }
      return(x)
    })
    colnames(dat2)=colnames(dat)
    rownames(dat2)=rownames(dat)
    dat=dat2
  }
  
  res = prcomp(dat, center = center, scale. = scale.)
  invisible(res)
  
 
}


