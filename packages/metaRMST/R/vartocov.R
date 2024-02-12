### Translates variance vector with NA's
## to meta-analysis covariance matrix

vartocov = function(var_vec) {
    # number of non-missing variances
    num.vars = sum(!is.na(var_vec))
    
    # set index of first missing variance
    na.index = num.vars + 1
    if(na.index > length(var_vec)) { 
      na.index = NA
    }
    
    # pattern: 
    # 1) elements 1 to k-1 once
      # 1) 1:num.vars
    # 2) element k repeated to first NA index
    # 3) other elements are NA (by definition)
    
    # 3) create matrix of NAs
    RMSTcov = matrix(NA, nrow=length(var_vec),
                     ncol=length(var_vec))
    
    # Fills in columns of matrix with Patterns 1 & 2
    if(!is.na(var_vec[1])){
    for (k in 1:num.vars) {
      pattern1 = var_vec[1:(k-1)]

      if(num.vars==1){pattern2 <- NA }else{pattern2 = rep(var_vec[k], num.vars - length(pattern1))}

      if(num.vars==1){RMSTcov[1:num.vars, k] <- pattern1 }else{ RMSTcov[1:num.vars, k] = c(pattern1, pattern2)}
    }
	} 
    return(RMSTcov)
}

