# Box Cox ----------------------------------------------------------------------

# Transformation: Box Cox

box_cox <- function(y, lambda = lambda) {
  lambda_cases <- function(y, lambda = lambda) {
    lambda_absolute <- abs(lambda)
    if (lambda_absolute <= 1e-12) {  #case lambda=0
      y <- log(y)
    } else {
      y <- ((y)^lambda - 1) / lambda
    }
    return(y)
  }
  y <- lambda_cases(y = y, lambda = lambda)
  
  return(y = y)
} # End box_cox



# Standardized transformation: Box Cox

geometric.mean <- function(x) { #for RMLE in the parameter estimation
  exp(mean(log(x)))
}

box_cox_std <- function(y, lambda) {
  gm <- geometric.mean(y)
  y <- if (abs(lambda) > 1e-12) {
    y <- (y^lambda - 1) / (lambda * ((gm)^(lambda - 1)))
  } else {
    y <- gm * log(y)
  }
  return(y)
}

# Back transformation: Box Cox
box_cox_back <- function(y, lambda) {
  
  lambda_cases_back <- function(y, lambda = lambda){
    if (abs(lambda) <= 1e-12) {   #case lambda=0
      y <-  exp(y) 
    } else {
      y <- (lambda * y + 1)^(1 / lambda) 
    }
    return(y = y)
  }
  y <- lambda_cases_back(y = y, lambda = lambda)
  
  return(y = y)
} #  End box_cox_back




#  Transformation: Box Cox shift

with_shift <- function(y, shift) {
  min <- min(y)
  if (min <= 0) {
    shift <- shift + abs(min(y)) + 1
  } else {
    shift <- shift
  }
  return(shift)
}

box_cox_shift <- function(y, lambda = lambda, shift = 0) {
  with_shift <- function(y, shift) {
    min <- min(y)
    if (min <= 0) {
      shift <- shift + abs(min(y)) + 1
    } else {
      shift <- shift
    }
    return(shift)
  }
  # Shift parameter
  shift <- with_shift(y = y, shift = shift)
  
  lambda_cases <- function(y, lambda = lambda) {
    lambda_absolute <- abs(lambda)
    if (lambda_absolute <= 1e-12) {  #case lambda=0
      y <- log(y + shift)
    } else {
      y <- ((y + shift)^lambda - 1) / lambda
    }
    return(y)
  }
  y <- lambda_cases(y = y, lambda = lambda)
  
  return(list(y = y, shift = shift))
} # End box_cox



# Standardized transformation: Box Cox

geometric.mean <- function(x) { #for RMLE in the parameter estimation
  exp(mean(log(x)))
}

box_cox_shift_std <- function(y, lambda) {
  min <- min(y)
  if (min <= 0) {
    y <- y - min + 1
  }
  
  gm <- geometric.mean(y)
  y <- if (abs(lambda) > 1e-12) {
    y <- (y^lambda - 1) / (lambda * ((gm)^(lambda - 1)))
  } else {
    y <- gm * log(y)
  }
  return(y)
}


# Back transformation: Box Cox shift
box_cox_shift_back <- function(y, lambda, shift = 0) {
  
  lambda_cases_back <- function(y, lambda = lambda, shift){
    if (abs(lambda) <= 1e-12) {   #case lambda=0
      y <-  exp(y) - shift
    } else {
      y <- (lambda * y + 1)^(1 / lambda) - shift
    }
    return(y = y)
  }
  y <- lambda_cases_back(y = y, lambda = lambda, shift = shift)
  
  return(y = y)
} #  End box_cox_back

# Modulus ----------------------------------------------------------------------

#  Transformation: Modulus
modul <- function(y, lambda = lambda) {
  u <- abs(y) + 1L
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  sign(y)*log(u)
  } else {
    yt <- sign(y)*(u^lambda - 1L)/lambda
  }
  return(y = yt)
}

# Standardized transformation: Modulus

modul_std <- function(y, lambda) {
  u <- abs(y) + 1L
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    zt <-  sign(y) * log(u) * geometric.mean(u) 
  } else {
    zt <- sign(y)*(u^lambda - 1L)/lambda * (1/geometric.mean(u)^(lambda - 1))
  }
  y <- zt
  
  return(y)
}


# Back transformation: Modulus
modul_back <- function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {
    y <- sign(y) * (exp(abs(y)) - 1)
  } else {
    y <- sign(y) * ((abs(y)*lambda + 1)^(1/lambda) - 1)
  }
  
  return(y = y)
}


# The Bickel-Doksum transformation ----------------------------------------------------------------------

#  Transformation: Bick-Doksum

Bick_dok <-  function(y, lambda = lambda) {
  
  if (lambda > 1e-12){
    yt <- (abs(y)^lambda * sign(y) - 1)/lambda
  }
  else{
    stop("lambda must be positive for the Bickel-Doksum transformation")
  }
  return(y = yt)
}


# Standardized transformation: Bick-Doksum

Bick_dok_std <- function(y, lambda) {
  
  yt <- Bick_dok(y, lambda)
  zt <- yt * (1 / geometric.mean(abs(y)^(lambda - 1)))
  y <- zt
  return(y)
}



# Back transformation: Bick-Doksum
Bick_dok_back <- function(y, lambda = lambda) {

  positivos <- which(y >= 0)
  y[positivos] <- (lambda * y[positivos] + 1)^(1 / lambda) 

  negativos <- which(y < 0)
  y[negativos] <- (-1) * ((-1) * (lambda * y[negativos] + 1))^(1 / lambda) 

  return(y = y)
}

# The Manly transformation ----------------------------------------------------------------------

# Transformation: Manly
Manly <-  function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  y
  } else {
    yt <- (exp(y*lambda) - 1L)/lambda
  }
  return(y = yt)
}

# Standardized transformation: Manly

Manly_std <- function(y, lambda) {
  lambda_absolute <- abs(lambda)
  yt <- Manly(y, lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    zt <-  y
  } else {
    zt <- yt/exp((mean(lambda*y)))
  }
  y <- zt
  return(y)
}

# Back transformation: Manly
Manly_back <- function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    y <- y  
  } else {
    y <- log(lambda * y + 1) / lambda
  }
  return(y = y)
}

# The dual transformation ----------------------------------------------------------------------

# Transformation: dual
Dual <-  function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  log(y)
  } else if (lambda > 1e-12){
    yt <- (y^(lambda) - y^(-lambda))/(2 * lambda)
  } else {
    stop("lambda can not be negative for the dual transformation")
  }
  return(y = yt)
}

# Standardized transformation: dual
Dual_std <- function(y, lambda) {
  yt <- Dual(y, lambda)
  
  zt <- if (abs(lambda) > 1e-12) {
    geo <- geometric.mean(y^(lambda -1) + y^(-lambda -1))
    zt <- yt * 2 / geo
  } else {
    zt <- geometric.mean(y) * log(y)
  }
  
  y <- zt
  
  return(y)
}

# Back transformation: dual
Dual_back <- function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if(lambda_absolute <= 1e-12)
  {
    y <- exp(y)
  }
  else
  {
    y <- (lambda * y + sqrt(lambda^2 * y^2 + 1))^(1/lambda)
  }
  
  return(y = y)
}

# The Yeo-Johnson transformation ----------------------------------------------------------------------

# Transformation: Yeo-Johnson
Yeo_john <-  function(y, lambda = lambda) {
  n <- length(y)
  yt <- rep(NA, n)
  negativos <- which(y < 0)
  positivos <- which(y >= 0)
  
  if(abs(lambda) <= 1e-12) {
    yt[positivos] <- log(y[positivos] + 1)
  } else {
    yt[positivos] <- ((y[positivos] + 1)^lambda - 1)/lambda
  }
  
  
  if(abs(lambda - 2) <= 1e-12) {
    yt[negativos] <- -log(-y[negativos] + 1)
  } else {
    yt[negativos] <- -((-y[negativos] + 1)^(2-lambda) - 1)/(2-lambda)
  }
  
  return(y = yt)
}


# Standardized transformation: Yeo-Johnson

Yeo_john_std <- function(y, lambda) {
  n <- length(y)
  zt <- rep(NA, n)
  negativos <- which(y < 0)
  positivos <- which(y >= 0)
  
  
  if(abs(lambda) <= 1e-12){
    gm <- geometric.mean(y[positivos] + 1)
    zt[positivos] <- gm * log(y[positivos] + 1)
  } 
  if (lambda != 0) {
    gm <- geometric.mean(y[positivos] + 1)
    zt[positivos] <- ((y[positivos] + 1)^lambda - 1)/(lambda*gm^(lambda - 1)) 
  }
  if(abs(lambda - 2) <= 1e-12) {
    gm <- geometric.mean(1 - y[negativos])
    zt[negativos] <- -log(-y[negativos] + 1) * gm
  } 
  
  if (lambda != 2) {
    gm <- geometric.mean(1 - y[negativos])
    zt[negativos] <- (-((-y[negativos] + 1)^(2 - lambda) - 1)/(2 - lambda))*(1/gm)
  }
  y <- zt
  return(y)
}



# Back transformation: Yeo-Johnson
Yeo_john_back <- function(y, lambda = lambda) {

  negativos <- which(y < 0)
  positivos <- which(y >= 0)
  
  lambda_absolute <- abs(lambda)
  if (lambda != 0) {
    y[positivos] <- ((y[positivos] * lambda + 1)^(1 / lambda)) - 1
  }
  if (lambda_absolute <= 1e-12) {
    y[positivos] <- exp(y[positivos]) - 1
  } 
  if (lambda != 2) {
    y[negativos] <- (-1) * ((y[negativos] * (lambda - 2) + 1)^(1/(2 - lambda)) - 1)
  } 
  if (lambda_absolute == 2) {
    y[negativos] <- (-1) * (exp(-y[negativos]) - 1)
  }
  
  return(y = y)
}

###################################### Neue Transformationen #######################################


#  Transformation: log_shift_opt


log_shift_opt <- function(y, lambda = lambda) {
  
  with_shift <-  function(y, lambda) {
    
    min <- min(y + lambda)
    if (min <= 0) {
      lambda <- lambda + abs(min) + 1
    } else {
      lambda <- lambda
    }
    return(lambda)
  }
  
  # Shift parameter
  lambda <- with_shift(y = y, lambda = lambda )
  
  log_trafo <- function(y, lambda = lambda) {
    y <- log(y + lambda)
    return(y)
  }
  y <- log_trafo(y = y, lambda = lambda)
  return(y)
} # End log_shift



# Standardized transformation: Log_shift_opt

geometric.mean <- function(x) { #for RMLE in the parameter estimation
  exp(mean(log(x)))
}

log_shift_opt_std <- function(y, lambda) {
  
  with_shift <-  function(y, lambda) {
    min <- min(y + lambda)
    if (min <= 0) {
      lambda <- lambda + abs(min(y)) + 1
    } else {
      lambda <- lambda
    }
    return(lambda)
  }
  
  # Shift parameter
  lambda <- with_shift(y = y, lambda = lambda )
  
  log_trafo_std <- function(y, lambda = lambda) {
    gm <- geometric.mean(y + lambda)
    y <- gm * log(y + lambda)
    return(y)
  }
  y <- log_trafo_std(y = y, lambda = lambda)
  return(y)
}

# Back transformation: log_shift_opt
log_shift_opt_back <- function(y, lambda) {
  log_shift_opt_back <- function(y, lambda = lambda){
    y <-  exp(y) - lambda
    return(y = y)
  }
  y <- log_shift_opt_back(y = y, lambda = lambda)
  return(y = y)
} #  End log_shift_opt

##############


#  Transformation: neg_log
neg_log <- function(y) {
  u <- abs(y) + 1L
  yt <-  sign(y)*log(u)
  
  return(y = yt)
}

# Standardized transformation: neg_log
neg_log_std <- function(y) {
  u <- abs(y) + 1L
  zt <- sign(y) * log(u) * geometric.mean(u)
  y <- zt
  return(y)
}

# Back transformation: neg_log
neg_log_back <- function(y) {
  y <- sign(y) * (exp(abs(y)) - 1)
  
  return(y)
}

# Transformation: log
Log <- function(y) {
  y <- box_cox(y, lambda = 0)
  return(y)
}

# Standardized transformation: log
Log_std <- function(y) {
  y <- box_cox_std(y, lambda = 0)
  return(y)
}


# Standardized transformation: log
Log_back <- function(y) {
  y <- box_cox_back(y, lambda = 0)
  return(y)
}


# Transformation: log
Log_shift <- function(y) {
  y <- box_cox_shift(y, lambda = 0)
  return(y)
}

# Standardized transformation: log
Log_shift_std <- function(y) {
  y <- box_cox_shift_std(y, lambda = 0)
  return(y)
}

# Standardized transformation: log
Log_shift_back <- function(y) {
  y <- box_cox_shift_back(y, lambda = 0)
  return(y)
}


# Transformation: Reciprocal
Reciprocal <- function(y)  {#lambda is fixed
  y <- box_cox(y, lambda = -1)
  return(y)
}

# Standardized transformation: Reciprocal

Reciprocal_std  <- function(y) {
  y <- box_cox_std(y, lambda = -1)
  return(y)
}

# Back transformation: Reciprocal
Reciprocal_back <- function(y) {
  box_cox_back(y, lambda = -1)
}



# Standardized transformation: squared_root_shift


sqrt_shift <- function(y, lambda = lambda) {
  
  
  with_shift <-  function(y, lambda) {
    
    min <- min(y + lambda)
    if (min <= 0) {
      lambda <- lambda + abs(min) + 1
    } else {
      lambda <- lambda
    }
    return(lambda)
  }
  
  # Shift parameter
  lambda <- with_shift(y = y, lambda = lambda )
  
  sqrt_trafo <- function(y, lambda = lambda) {
    y <- sqrt(y + lambda)
    return(y)
  }
  y <- sqrt_trafo(y = y, lambda = lambda)
  return(y)
} # End log_shift



# Standardized transformation: sqrt_shift

geometric.mean <- function(x) { #for RMLE in the parameter estimation
  exp(mean(log(x)))
}

sqrt_shift_std <- function(y, lambda) {
  
  with_shift <-  function(y, lambda) {
    min <- min(y + lambda)
    if (min <= 0) {
      lambda <- lambda + abs(min) + 1
    } else {
      lambda <- lambda
    }
    return(lambda)
  }
  
  # Shift parameter
  lambda <- with_shift(y = y, lambda = lambda )
  
  sqrt_trafo_std <- function(y, lambda = lambda) {
    gm <- geometric.mean(y + lambda)
    y <- gm * sqrt(y + lambda)
    return(y)
  }
  y <- sqrt_trafo_std(y = y, lambda = lambda)
  return(y)
}

# Back transformation: log_shift
sqrt_shift_back <- function(y, lambda) {

  sqrt_shift_back <- function(y, lambda = lambda){
    y <-  y^2 - lambda
    return(y = y)
  }
  y <- sqrt_shift_back(y = y, lambda = lambda)
  return(y = y)
} #  End sqrt_shift




# Transformation: Gpower

gPower <-  function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  log(y + sqrt(y^2 + 1))
  } else if (lambda_absolute > 1e-12) {
    yt <- ((y + sqrt(y^2 + 1))^lambda - 1)/lambda
  }
  return(y = yt)
}

# Standardized transformation: Gpower
gPower_std <- function(y, lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    zt <-  log(y + sqrt(y^2 + 1)) * sqrt(geometric.mean(1 + y^2))
    
  } else if (lambda_absolute > 1e-12) {
    zt <- (((y + sqrt(y^2 + 1))^lambda - 1)/lambda) / geometric.mean((y + sqrt(y^2 + 1))^(lambda - 1) * (1 + (y/sqrt(y^2 +1))))
  }
  
  y <- zt
  
  return(y)
}

# Back transformation: Gpower
gPower_back <- function(y, lambda = lambda) {
  
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <- (-(1 - exp(y*2))) / (2 * exp(y))
    
  } else if (lambda_absolute > 1e-12) {
    A <- (y * lambda + 1)^(1 / lambda)
    yt <- (-(1 - A^2)) / (2*A)
  }
  return(y = yt)
  
}



# Glog
g_log <- function(y) {
  
  yt <-  log(y + sqrt(y^2 + 1))
  
  return(y = yt)
}


# Standardized transformation
g_log_std <- function(y) {
  
  yt <-  log(y + sqrt(y^2 + 1)) * sqrt(geometric.mean(1 + y^2))
  
  return(y = yt)
}

# Back-transformation
g_log_back <- function(y) {
  
  yt <- (-(1 - exp(y*2))) / (2 * exp(y))
  
  return(y = yt)
}
