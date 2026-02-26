


# Discretizing a sample of n survival times into sub-intervals with no. of 'binCount' subjects in each sub-interval
discrBinNA <- function( survdat, binCount, tie )
{
  X <- survdat[ , 1 ] # survival times
  len <- length( X )  # sample size
  binid <- seq( 1, len, binCount )
  nbins <- length( binid )
  sortX <- sort( X, index.return = TRUE )       # sort the data into ascending order
  sX <- sortX$x
  binedg <- sX[ binid ]

  if( tie == 'No' )
  {

    if( binCount == 1 )
    {
      binwv <- binedg[ 1:nbins ] - c( 0, binedg[ 1:( nbins - 1 ) ] ) #binwv: bin widths
      binedg <- c( 0, binedg )
      binID <- rank( X ) #binID: bin IDs
    }
    else
    {

      i <- 0
      while( i < ( nbins - 1 ) )
      {

        i <- i + 1
        ntied <- sum( binedg[ i ] == binedg )
        if( ( i + ntied ) <= nbins )
        {
          binedg <- c( binedg[ 1:i ], binedg[ ( i + ntied ):nbins ] )
        }
        else
        {
          binedg <- c( binedg[ 1:i ] )
        }
        nbins <- length( binedg )

      }

      if( binedg[ nbins ] == max( X ) )
      {
        nbins <- nbins - 1
        binedg <- binedg[ 1:nbins ]
      }
      binedg[ 1 ] <- min( X )
      binedg[ nbins + 1 ] <- max( X ) + ( 1e-2 )*( binedg[ nbins ] - binedg[ nbins - 1 ] )

      binedg[ nbins ] <- binedg[ nbins ]*( 1 - ( 1e-5 ) )

      binwv <- binedg[ 2:( nbins + 1 ) ] - binedg[ 1:nbins ]
      binID <- rep( 0, len )
      for ( i in 1:len )
      {
        for ( j in 1:( nbins ) )
        {
          if( X[ i ] < binedg[ j + 1 ] & X[ i ] >= binedg[ j ] )
          {
            binID[ i ] <- j
          }
        }
      }

    }
  }
  else
  {
    i <- 0
    while( i < ( nbins - 1 ) )
    {
      i <- i + 1
      ntied <- sum( binedg[ i ] == binedg )
      if( ( i + ntied ) <= nbins )
      {
        binedg <- c( binedg[ 1:i ], binedg[ ( i + ntied ):nbins ] )
      }
      else
      {
        binedg <- c( binedg[ 1:i ] )
      }
      nbins <- length( binedg )

    }
    if( binedg[ nbins ] == max( X ) )
    {
      nbins <- nbins - 1
      binedg <- binedg[ 1:nbins ]
    }
    binedg[ 1 ] <- min( X )
    binedg[ nbins + 1 ] <- max( X ) + ( 1e-2 )
    binedg[ nbins ] <- binedg[ nbins ]*( 1 - ( 1e-5 ) )
    binwv <- binedg[ 2:( nbins + 1 ) ] - binedg[ 1:nbins ]
    binID <- rep( 0, len )
    for ( i in 1:len )
    {
      for ( j in 1:( nbins ) )
      {
        if( X[ i ] < binedg[ j + 1 ] & X[ i ] >= binedg[ j ] )
        {
          binID[ i ] <- j
        }
      }
    }
  }
  classify <- cbind( binwv )
  return( list( discretize = classify, ID = binID, binedg = binedg ) )
}

#piecewise constant basis function for the non-parametric baseline hazard
psi <- function( ID )
{
  n <- length( ID )
  m <- max( ID )
  psix <- matrix( 0, n, m )
  for( i in 1:n )
  {
    psix[ i, ID[ i ] ] <- 1
  }
  return( psix )
}

#piecewise constant cumulative basis function for the non-parametric baseline hazard
Psi <- function( ID, binwv )
{
  n <- length( ID )
  m <- max( ID )
  Psix <- matrix( 0, n, m )
  for( i in 1:n )
  {
    Psix[ i, 1:ID[ i ] ] <- 1*binwv[ 1:ID[ i ] ]
  }
  return( Psix )
}

#Computing the baseline hazard given the basis functions and the corresponding regression covariate coefficients
baseHaz <- function( bslh, psix )
{
  #bslh:  regression coefficients for the baseline hazard
  #psix:  basis function for the non-parametric baseline hazard
  h0 <- ( psix%*%bslh )
  return( h0 )
}


#Computing the cumulative baseline hazard given the basis functions and the corresponding regression covariate coefficients
baseCumm <- function( bslh, Psix )
{
  #bslh:  regression coefficients for the baseline hazard
  #Psix: cumulative basis function for the non-parametric baseline hazard
  H0 <- ( Psix%*%bslh )
  return( H0 )
}

# Computing the cumulative and survival functions given the basis functions of baseline hazard and the regression covariate coefficients
coxCumm <- function( bslh, Psix, coefs, cova )
{
  #bslh:  regression coefficients for the baseline hazard
  #Psijx: cumulative basis function for the non-parametric baseline hazard
  #coefs: regression coefficients
  #cova: covariates
  #ID: bin ID
  H <- ( Psix%*%bslh )*( exp( cova%*%coefs ) )
  return( H )
}
coxSurv <- function( bslh, Psix, coefs, cova )
{
  S <- ( exp( -( Psix%*%bslh ) ) )^( exp( cova%*%coefs ) )
  return( S )
}

# Initial estimates of theta (piecewise constant estimate of h_{0t}) based on independent censoring assumption
theta_initial <- function( del, psix, Psix, beta0, cova )
{
  n <- dim( psix )[ 1 ]
  m <- dim( psix )[ 2 ]
  eregt <- exp( cova%*%beta0 )
  theta <- colSums( matrix( del, n, m )*psix )/( colSums( matrix( eregt, n, m )*Psix ) + 1e-5 )
  return( theta )
}

# Initial estimates of gamma (piecewise constant estimate of h_{0c}) based on independent censoring assumption
gamma_initial <- function( eta, psix, Psix, phi0, cova )
{
  n <- dim( psix )[ 1 ]
  m <- dim( psix )[ 2 ]
  eregc <- exp( cova%*%phi0 )
  gamma <- colSums( matrix( eta, n, m )*psix )/( colSums( matrix( eregc, n, m )*Psix ) + 1e-5 )
  return( gamma )
}



# Independent copula, e.g. C(u, v; alpha)=u*v, where u and v are the marginal survival functions of T and C respectively
IndependentCopula <- function( u, v )
{
  Co <- u*v       #C(u, v; alpha)
  return( Co )
}

# First and second derivative of Independent copula
dC_ind <- function( u, v )
{
  Ct <- v    #dC(u, v; alpha)/du
  Cc <- u  #dC(u, v; alpha)/dv
  Ctc <- 1 #d( dC(u, v; alpha)/du )/dv
  Ctt <- 0  #d( dC(u, v; alpha)/du )/du
  Ccc <- 0   #d( dC(u, v; alpha)/dv )/dv
  dC <- cbind( Ct, Cc, Ctc, Ctt, Ccc )
  return( dC )
}

# Third derivative of Independent copula
dC3_ind <- function( u, v )
{
  Cttt = 0      # d( d( dC(u, v; alpha)/du )/du )/du
  Cccc = 0      # d( d( dC(u, v; alpha)/dv )/dv )/dv
  Ctct = 0      # d( d( dC(u, v; alpha)/du )/dv )/du
  Ctcc = 0      # d( d( dC(u, v; alpha)/du )/dv )/dv
  dC3 <- cbind( Cttt, Cccc, Ctct, Ctcc )
  return( dC3 )
}

# Clayton copula
ClaytonCopula <- function( u, v, alpha )
{
  Co <- ( u^( -alpha ) + v^( -alpha ) -1 )^( -1/alpha )  #C(u, v; alpha)
  return( Co )
}

# First and second derivative of Clayton copula
dC_clay <- function( u, v, alpha )
{
  Ct <- ( u^( -alpha ) + v^( -alpha ) - 1 )^( ( -1/alpha ) - 1 ) * u^( -alpha - 1 )   #dC(u, v; alpha)/du
  Cc <- ( u^( -alpha ) + v^( -alpha ) - 1 )^( ( -1/alpha ) - 1 ) * v^( -alpha - 1 )    #dC(u, v; alpha)/dv
  Ctc <- ( u*v )^( -alpha - 1 ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( ( -1/alpha ) - 2 ) * ( 1 + alpha )      #d( dC(u, v; alpha)/du )/dv
  Ctt <- ( alpha + 1 )*( u^( -alpha ) + v^( -alpha ) - 1 )^( ( -1/alpha ) - 2 ) * u^( -alpha - 2 ) * ( 1 - v^( -alpha ) )   #d( dC(u, v; alpha)/du )/du
  Ccc <- ( alpha + 1 )*( u^( -alpha ) + v^( -alpha ) -1 )^( ( -1/alpha ) - 2 ) * v^( -alpha - 2 ) * ( 1 - u^( -alpha ) )   #d( dC(u, v; alpha)/dv )/dv
  dC <- cbind( Ct, Cc, Ctc, Ctt, Ccc )
  return( dC )
}

# Third derivative of Clayton copula
dC3_clay <- function( u, v, alpha )
{
  # d( d( dC(u, v; alpha)/du )/du )/du
  Cttt <- -( 1 + alpha ) * ( 1 - v^( -alpha ) ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( -1/alpha - 2 ) * ( alpha + 2 ) * ( u^( -alpha - 3 ) ) + ( alpha + 1 ) * ( 1 - v^( -alpha ) ) * ( u^( -alpha - 2 ) ) * ( 1 + 2*alpha ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( -1/alpha -3 ) * ( u^( -alpha - 1 ) )
  # d( d( dC(u, v; alpha)/dv )/dv )/dv
  Cccc <- -( 1 + alpha ) * ( 1 - u^( -alpha ) ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( -1/alpha - 2 ) * ( alpha + 2 ) * ( v^( -alpha - 3 ) ) + ( alpha + 1 ) * ( 1 - u^( -alpha ) ) * ( v^( -alpha - 2 ) ) * ( 1 + 2*alpha ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( -1/alpha -3 ) * ( v^( -alpha - 1 ) )
  # d( d( dC(u, v; alpha)/du )/dv )/dv
  Ctcc <- ( 1 + alpha ) * ( 1 + 2*alpha ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( -1/alpha - 3 ) * ( v^( -alpha-1 ) ) * ( u*v )^( -alpha - 1 ) - ( 1 + alpha )^( 2 ) * ( u^(-alpha) + v^(-alpha) - 1 )^( -1/alpha - 2 ) * ( u*v )^( -alpha - 2 ) * u
  # d( d( dC(u, v; alpha)/du )/dv )/du
  Ctct <- ( 1 + alpha ) * ( 1 + 2*alpha ) * ( u^( -alpha ) + v^( -alpha ) - 1 )^( -1/alpha - 3 ) * ( u^( -alpha-1 ) ) * ( u*v )^( -alpha - 1 ) - ( 1 + alpha )^( 2 ) * ( u^(-alpha) + v^(-alpha) - 1 )^( -1/alpha - 2 ) * ( u*v )^( -alpha - 2 ) * v
  dC3 <- cbind( Cttt, Cccc, Ctct, Ctcc )
  return( dC3 )
}

# Gumbel copula
GumbelCopula <- function( u, v, alpha )
{
  Co <- exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) )     #C(u, v; alpha)
  return( Co )
}

# First and second derivative of Gumbel copula
dC_gumbel <- function( u, v, alpha )
{
  #dC(u, v; alpha)/du
  Ct <- ( 1/u ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( ( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha - 1 ) ) * ( ( -log( u ) )^( alpha - 1 ) )
  #dC(u, v; alpha)/dv
  Cc <- ( 1/v ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( ( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha - 1 ) ) * ( ( -log( v ) )^( alpha - 1 ) )
  #d( dC(u, v; alpha)/du )/dv
  Ctc <- ( 1/( u*v ) ) * ( log( u ) * log( v ) )^( alpha - 1 ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( ( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha - 2 ) ) * ( ( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) + ( alpha - 1 ) )
  #d( dC(u, v; alpha)/du )/du
  Ctt <- ( 1/( u^2 ) ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 2/alpha - 2 ) * ( -log( u ) )^( 2*alpha - 2 ) - ( 1/( u^2 ) ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( 1 - alpha ) * ( ( -log( u ) )^( alpha ) + ( -log( v ) )^ (alpha) )^(1/alpha - 2) * ( -log(u) )^(2*alpha-2) - (1/(u^2)) * ( exp( -( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha) ) ) * ( alpha-1 ) * ( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha - 1) * ( -log(u) )^(alpha-2) - (1/(u^2)) * ( exp( -( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha) ) ) * ( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha - 1) * ( -log(u) )^(alpha-1)
  #d( dC(u, v; alpha)/dv )/dv
  Ccc <- ( 1/( v^2 ) ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 2/alpha - 2 ) * ( -log( v ) )^( 2*alpha - 2 ) - ( 1/( v^2 ) ) * ( exp( -( ( -log( u ) )^( alpha ) + ( -log( v ) )^( alpha ) )^( 1/alpha ) ) ) * ( 1 - alpha ) * ( ( -log( u ) )^( alpha) + ( -log( v ) )^(alpha) )^(1/alpha - 2) * ( -log(v) )^(2*alpha-2) - (1/(v^2)) * ( exp( -( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha) ) ) * ( alpha-1 ) * ( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha - 1) * ( -log(v) )^(alpha-2) - (1/(v^2)) * ( exp( -( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha) ) ) * ( ( -log(u) )^(alpha) + ( -log(v) )^(alpha) )^(1/alpha - 1) * ( -log(v) )^(alpha-1)
  dC <- cbind( Ct, Cc, Ctc, Ctt, Ccc )
  return(dC)
}

# Third derivative of Gumbel copula
dC3_gumbel <- function(u, v, alpha)
{
  Cttt1 <- ( -2 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2) ) * ( (-log(u))^(2*alpha-2) ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(3/alpha - 3) ) * ( (-log(u))^(3*alpha-3) ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(u))^(3*alpha-3) ) * ( 2*alpha - 2 ) + ( 1/ u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2) ) * ( (-log(u))^(2*alpha-3) ) * ( 2 - 2*alpha )
  Cccc1 <- ( -2 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2) ) * ( (-log(v))^(2*alpha-2) ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(3/alpha - 3) ) * ( (-log(v))^(3*alpha-3) ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(v))^(3*alpha-3) ) * ( 2*alpha - 2 ) + ( 1/ v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2) ) * ( (-log(v))^(2*alpha-3) ) * ( 2 - 2*alpha )
  Cttt2 <- -( ( -2 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(u))^(2*alpha-2) ) * ( 1 - alpha ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(u))^(3*alpha-3) ) * ( 1 - alpha ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 3) ) * ( (-log(u))^(3*alpha-3) ) * ( 1 - alpha ) * ( 2*alpha - 1 ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(u))^(2*alpha-3) ) * ( 1 - alpha ) * ( 2 - 2*alpha ) )
  Cccc2 <- -( ( -2 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(v))^(2*alpha-2) ) * ( 1 - alpha ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(v))^(3*alpha-3) ) * ( 1 - alpha ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 3) ) * ( (-log(v))^(3*alpha-3) ) * ( 1 - alpha ) * ( 2*alpha - 1 ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(v))^(2*alpha-3) ) * ( 1 - alpha ) * ( 2 - 2*alpha ) )
  Cttt3 <- -( ( -2 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(u))^(1*alpha-2) ) * ( alpha - 1 ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * (( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2)  ) * ( (-log(u))^(2*alpha-3) ) * ( alpha-1 ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(u))^(2*alpha-3) ) * ( (alpha-1)^2 ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(u))^(1*alpha-3) ) * ( alpha - 1 ) * ( 2 - alpha ) )
  Cccc3 <- -( ( -2 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(v))^(1*alpha-2) ) * ( alpha - 1 ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * (( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2)  ) * ( (-log(v))^(2*alpha-3) ) * ( alpha-1 ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(v))^(2*alpha-3) ) * ( (alpha-1)^2 ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(v))^(1*alpha-3) ) * ( alpha - 1 ) * ( 2 - alpha ) )
  Cttt4 <- -( ( -2 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(u))^(1*alpha-1) ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2) ) * ( (-log(u))^(2*alpha-2) ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(u))^(2*alpha-2) ) * ( alpha - 1 ) + ( 1 / u^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(u))^(1*alpha-2) ) * ( 1 - alpha ) )
  Cccc4 <- -( ( -2 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(v))^(1*alpha-1) ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 2) ) * ( (-log(v))^(2*alpha-2) ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( (-log(v))^(2*alpha-2) ) * ( alpha - 1 ) + ( 1 / v^3 ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 1) ) * ( (-log(v))^(1*alpha-2) ) * ( 1 - alpha ) )
  # d( d( dC(u, v; alpha)/du )/du )/du
  Cttt <- Cttt1 + Cttt2 + Cttt3 + Cttt4
  # d( d( dC(u, v; alpha)/dv )/dv )/dv
  Cccc <- Cccc1 + Cccc2 + Cccc3 + Cccc4
  Ctct1 <- ( -1 / ( u^2 * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Cctc1 <- ( -1 / ( v^2 * u ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Ctct2 <- ( 1 / ( u^2 * v ) ) * ( log(v) ) * ( alpha-1 ) * ( ( log(u) * log(v) )^(alpha-2) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Cctc2 <- ( 1 / ( v^2 * u ) ) * ( log(u) ) * ( alpha-1 ) * ( ( log(u) * log(v) )^(alpha-2) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 2) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Ctct3 <- ( 1 / ( u * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(u))^(alpha-1) ) * ( 1/u ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Cctc3 <- ( 1 / ( u * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(v))^(alpha-1) ) * ( 1/v ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Ctct4 <- ( 1 / ( u * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( 2*alpha - 1 ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 3) ) * ( (-log(u))^(alpha-1) ) * ( 1/u ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Cctc4 <- ( 1 / ( u * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( 2*alpha - 1 ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha - 3) ) * ( (-log(v))^(alpha-1) ) * ( 1/v ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) + (alpha-1) )
  Ctct5 <- ( 1 / ( u * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(u))^(alpha-1) ) * ( -1/u )
  Cctc5 <- ( 1 / ( u * v ) ) * ( ( log(u) * log(v) )^(alpha-1) ) * ( exp( -( (-log(u))^(alpha) + (-log(v))^(alpha) )^(1/alpha) ) ) * ( ( (-log(u))^(alpha) + (-log(v))^(alpha) )^(2/alpha - 3) ) * ( (-log(v))^(alpha-1) ) * ( -1/v )
  # d( d( dC(u, v; alpha)/du )/dv )/du
  Ctct <- Ctct1 + Ctct2 + Ctct3 + Ctct4 + Ctct5
  # d( d( dC(u, v; alpha)/dv )/du )/dv
  Cctc <- Cctc1 + Cctc2 + Cctc3 + Cctc4 + Cctc5
  dC3 <- cbind(Cttt, Cccc, Ctct, Cctc)
  return(dC3)
}

# Frank copula
FrankCopula<-function(u, v, alpha)
{
  Co <- ( 1/alpha )*log( 1 + ( exp( alpha*u ) - 1 )*( exp( alpha*v ) - 1 )/( exp( alpha ) - 1 ) )  #C(u, v; alpha)
  return(Co)
}

# First and second derivative of Frank copula
dC_frank <- function(u, v, alpha)
{
  #dC(u, v; alpha)/du
  Ct <- ( exp( alpha*(u+v) ) - exp( alpha*u ) ) / ( exp( alpha ) + exp( alpha*(u+v) ) - exp( alpha*u ) - exp( alpha*v ) )
  #dC(u, v; alpha)/dv
  Cc <- ( exp( alpha*(u+v) ) - exp( alpha*v ) ) / ( exp( alpha ) + exp( alpha*(u+v) ) - exp( alpha*u ) - exp( alpha*v ) )
  #d( dC(u, v; alpha)/du )/dv
  Ctc <- ( alpha * exp( alpha*(u+v) ) *  ( exp( alpha ) - 1) ) / ( exp( alpha ) + exp( alpha*(u+v) ) - exp( alpha*u ) - exp( alpha*v ) )^(2)
  #d( dC(u, v; alpha)/du )/du
  Ctt <- ( alpha * exp( alpha*u ) * ( exp( alpha*v ) - 1 ) * ( exp( alpha ) - exp( alpha*v ) ) ) / ( exp( alpha ) + exp( alpha*(u+v) ) - exp( alpha*u ) - exp( alpha*v ) )^(2)
  #d( dC(u, v; alpha)/dv )/dv
  Ccc <- ( alpha * exp( alpha*v ) * ( exp( alpha*u ) - 1 ) * ( exp( alpha ) - exp( alpha*u ) ) ) / ( exp( alpha ) + exp( alpha*(u+v) ) - exp( alpha*u ) - exp( alpha*v ) )^(2)
  dC <- cbind(Ct, Cc, Ctc, Ctt, Ccc)
  return(dC)
}

# Third derivative of Frank copula
dC3_frank <- function(u, v, alpha)
{
  # d( d( dC(u, v; alpha)/du )/du )/du
  Cttt <- ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) )^(2) * ( alpha^2 ) * ( exp(alpha*u) ) * ( exp(alpha*v) - 1 ) * ( exp(alpha) - exp(alpha*v) ) - 2*( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) ) * ( exp(alpha*(u+v)) - exp(alpha*u) ) * ( alpha^2 ) * ( exp(alpha*u) ) * ( exp(alpha*v) - 1 ) * ( exp(alpha) - exp(alpha*v) ) ) / ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) - exp(alpha*v) )^(4) )
  # d( d( dC(u, v; alpha)/dv )/dv )/dv
  Cccc <- ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) )^(2) * ( alpha^2 ) * ( exp(alpha*v) ) * ( exp(alpha*u) - 1 ) * ( exp(alpha) - exp(alpha*u) ) - 2*( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) ) * ( exp(alpha*(u+v)) - exp(alpha*v) ) * ( alpha^2 ) * ( exp(alpha*v) ) * ( exp(alpha*u) - 1 ) * ( exp(alpha) - exp(alpha*u) ) ) / ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) - exp(alpha*v) )^(4) )
  # d( d( dC(u, v; alpha)/du )/dv )/dv
  Ctcc <- ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) )^(2) * ( alpha^2 ) * ( exp(alpha) - 1 ) * ( exp( alpha*(u+v) ) ) - 2*( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) ) * ( exp(alpha*(u+v)) - exp(alpha*v) ) * ( alpha^2 ) * ( exp(alpha) - 1 ) * ( exp( alpha*(u+v) ) ) ) / ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) - exp(alpha*v) )^(4) )
  # d( d( dC(u, v; alpha)/du )/dv )/du
  Ctct <- ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) )^(2) * ( alpha^2 ) * ( exp(alpha) - 1 ) * ( exp( alpha*(u+v) ) ) - 2*( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) -exp(alpha*v) ) * ( exp(alpha*(u+v)) - exp(alpha*u) ) * ( alpha^2 ) * ( exp(alpha) - 1 ) * ( exp( alpha*(u+v) ) ) ) / ( ( exp(alpha) + exp(alpha*(u+v)) - exp(alpha*u) - exp(alpha*v) )^(4) )
  dC3 <- cbind(Cttt, Cccc, Ctct, Ctcc)
  return(dC3)
}


# Penality functions

#For piecewise constant

# Second order difference
mat2 <- function( psix, X, eps )
{
  m <- dim( psix )[2]
  spsix <- psix[ sort( X, index.return = T )$ix, ]
  R <- t( diff( spsix, lag = 1, difference = 2 ) )%*%diff( spsix, lag = 1, difference = 2 )
  diag( R ) <- diag( R ) + eps
  return( R )
}

# First order difference
#For piecewise constant
mat1 <- function(psix, X, eps)
{
  m<-dim(psix)[2]
  spsix<-psix[sort(X, index.return=T)$ix,]
  R<-t(diff(spsix, lag=1, difference=1))%*%diff(spsix, lag=1, difference=1)
  diag(R)<-diag(R)+eps
  return(R)
}

#For m-spline
penalty_mspl<-function(numSp, ordSp, IntKnt, bryKnt)
{
  R<-matrix(0, nrow=numSp, ncol=numSp)
  xknots <- c(rep(min(bryKnt), ordSp), IntKnt, rep(max(bryKnt), ordSp))
  for (ii in 1:numSp)
  {
    for (jj in ii:numSp){
      if (jj - ii<ordSp){
        kntset <- xknots[xknots>=xknots[jj] & xknots<=xknots[ii+ordSp]];
        kntsum <- 0;
        for (kk in 1:(length(kntset)-1))
        {
          kntsum <- kntsum + mSpline(kntset[kk], knots=IntKnt, degree=ordSp-1, intercept=T,
                                    Boundary.knots=bryKnt, derivs=ordSp-2)[ii]*mSpline(kntset[kk],                                         knots=IntKnt, degree=ordSp-1, intercept=T, Boundary.knots=bryKnt,                                      derivs=ordSp-2)[jj]*(kntset[kk+1]-kntset[kk]);
        }
        R[ii, jj] <- kntsum;
      }
    }
  }
  R[lower.tri(R, diag = FALSE)] <- t(R)[lower.tri(R, diag = FALSE)]
  return(R)
}

# Penalized log likelihood function
penlogreg_dep<-function(del, eta, bslht, bslhc, h0t, h0c, regt, regc, Hcoxt, Hcoxc, Co, dC, Rt, Rc, smpart, smparc)
{
  #Co=C(u, v; alpha)
  C_t<-dC[,1]  #dC(u, v; alpha)/du
  C_c<-dC[,2]  #dC(u, v; alpha)/dv, where u=exp(-Hcoxt) and v-exp(-Hcoxc)
  # del: failure indicator, eta: dependent censoring indicator
  #cova: regression covariates
  #h0t: baseline hazard for failure time,
  #h0c: baseline hazard for dependent censoring time
  #regt: cova%*%beta, regc: cova%*%phi, Hcoxt: Cox proportional cumulative hazard of failure time, Hcoxc: Cox proportional cumumlative hazard of dependent censoring time
  pl <- sum( regt[del==1] ) + sum( regc[eta==1] ) + sum( log(h0t[del==1]) ) + sum( log(h0c[eta==1]) ) - sum( Hcoxt[del==1] ) - sum( Hcoxc[eta==1] ) + sum( log( C_t[del==1] ) ) + sum( log( C_c[eta==1] ) ) + sum( log( Co[del==0 & eta==0] ) ) - 0.5*smpart*t(bslht)%*%Rt%*%bslht - 0.5*smparc*t(bslhc)%*%Rc%*%bslhc
  return(pl)
}

# Score function of penalized log likelihood respect to theta
U_theta<-function(psix, Psix, bslht, h0t, eregt, avi, del, eta, dC, Co, smpart, Rt)
{
  n<-dim(psix)[1];m=dim(psix)[2]
  C_tt<-dC[,4] #d( dC(u, v; alpha)/du )du
  C_tc<-dC[,3] #d( dC(u, v; alpha)/du )dv, where u=S_T(x_i), v=S_C(x_i)
  C_t<-dC[,1]
  C_c<-dC[,2]
  Hi1<-del*C_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  SiT<-avi
  sc <- colSums( matrix( ( del/h0t ),n,m )*psix - matrix( ( ( del+Hi1*SiT )*eregt ),n,m )*Psix ) - smpart*Rt%*%bslht
  return(sc)
}

# Score function of penalized likelihood respect to gamma
U_gamma<-function(psix, Psix, bslhc, h0c, eregc, bvi, del, eta, dC, Co, smparc, Rc)
{
  n<-dim(psix)[1];m=dim(psix)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_cc<-dC[,5]
  Hi2<-eta*C_cc/C_c+del*C_tc/C_t+(1-del-eta)*C_c/Co
  SiC<-bvi
  sc <- colSums( matrix( ( eta/h0c ),n,m )*psix - matrix( ( ( eta+Hi2*SiC )*eregc ),n,m )*Psix ) - smparc*Rc%*%bslhc
  return(sc)
}

# Score function of penalized log likelihood respect to beta
U_beta<-function(p, cova, Htcox, avi, del, eta, dC, Co)  #Htcox=H_T(x_i)
{
  C_tt<-dC[,4]
  C_tc<-dC[,3]
  C_t<-dC[,1]
  C_c<-dC[,2]
  Htcoxz<-( Htcox%*%t( rep(1,p) ) )*cova
  Ctta<-( ( C_tt*avi )%*%t( rep(1,p) ) )*Htcoxz/( C_t%*%t( rep(1,p) ) )
  Ccta<-( ( C_tc*avi )%*%t( rep(1,p) ) )*Htcoxz/( C_c%*%t( rep(1,p) ) )
  Cta<-( ( C_t*avi )%*%t( rep(1,p) ) )*Htcoxz/( Co%*%t( rep(1,p) ) )
  if(p==1) #only one covariate
  {
    sc <- sum(cova[del==1,]) - sum(Htcoxz[del==1,]) - sum(Ctta[del==1,]) - sum(Ccta[eta==1,]) - sum(Cta[del==0 & eta==0,])
  }
  else
  {
    sc <- apply( rbind(cova[del==1,], rep(0, p)), 2, sum ) - apply( rbind(Htcoxz[del==1,], rep(0, p)), 2, sum ) - apply( rbind(Ctta[del==1,], rep(0, p)), 2, sum ) - apply( rbind(Ccta[eta==1,], rep(0, p)), 2, sum ) - apply( rbind(Cta[del==0 & eta==0,], rep(0, p)), 2, sum )
  }
  return(sc)
}

# Score function of penalized log likelihood respect to phi
U_phi<-function(p, cova, Hccox, bvi, del, eta, dC, Co) #Hccox=H_C(x_i)
{
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_cc<-dC[,5]
  Hccoxz<-((Hccox)%*%t( rep(1,p) ) )*cova
  Ctcb<-((C_tc*bvi)%*%t( rep(1,p) ))*Hccoxz/(C_t%*%t(rep(1,p)))
  Cccb<-((C_cc*bvi)%*%t(rep(1,p)))*Hccoxz/(C_c%*%t(rep(1,p)))
  Ccb<-((C_c*bvi)%*%t(rep(1,p)))*Hccoxz/(Co%*%t(rep(1,p)))
  if(p==1)
  {
    sc <- sum(cova[eta==1,]) - sum(Hccoxz[eta==1,]) - sum(Ctcb[del==1,]) - sum(Cccb[eta==1,]) - sum(Ccb[del==0 & eta==0,])
  }
  else
  {
    sc <- apply( rbind(cova[eta==1,], rep(0, p)), 2, sum ) - apply( rbind(Hccoxz[eta==1,], rep(0, p)), 2, sum ) - apply( rbind(Ctcb[del==1,], rep(0, p)), 2, sum ) - apply( rbind(Cccb[eta==1,], rep(0, p)), 2, sum ) - apply( rbind(Ccb[del==0 & eta==0,], rep(0, p)), 2, sum )
  }
  return(sc)
}


# Hessian function respect to beta
Hess_beta<-function(p, Co, dC, dC3, cova, Htcox, Stcox, del, eta) #Stcox=S_T(x_i)
{
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  ddHt<-Htcox
  ddlnCt<-( ( -( C_ttt )*( Stcox*Htcox )^2 - ( C_tt*Stcox )*( Htcox )^2 + ( C_tt*Stcox*Htcox ) )*( C_t ) + ( C_tt*Stcox*Htcox )^2 )/(C_t^2)
  ddlnCc<-( ( -( C_tct )*( Stcox*Htcox )^2 - ( C_tc*Stcox )*( Htcox )^2 + ( C_tc*Stcox*Htcox ) )*( C_c ) + ( C_tc*Stcox*Htcox )^2 )/(C_c^2)
  ddlnC<-( ( -( C_tt )*( Stcox*Htcox )^2 - ( C_t*Stcox )*( Htcox )^2 + ( C_t*Stcox*Htcox ) )*( Co ) + ( C_t*Stcox*Htcox )^2 )/(Co^2)
  Ibb <- matrix(0, p, p)
  for(i in 1:p) # i repesents the row number, and j represents the column number of each hessian matrix
  {
    zi<-cova[,i]
    for(j in 1:p)
    {
      zj<-cova[,j]
      ddHtzz<-ddHt*zi*zj
      ddlnCtzz<-ddlnCt*zi*zj
      ddlnCczz<-ddlnCc*zi*zj
      ddlnCzz<-ddlnC*zi*zj
      Ibb[i,j]<- -sum( ddHtzz[del==1] )-sum( ddlnCtzz[del==1] )-sum( ddlnCczz[eta==1] )-sum( ddlnCzz[del==0 & eta==0] )
    }
  }
  return(Ibb)
}

# Hessian function respect to beta and phi
Hess_phi_beta<-function(p, Co, dC, dC3, cova, Htcox, Hccox, Stcox, Sccox, del, eta)
{
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  ddlnCt<-( ( -C_tct*Stcox*Sccox*Htcox*Hccox )*( C_t ) + ( C_tc*C_tt*Stcox*Sccox*Htcox*Hccox ) )/(C_t^2)
  ddlnCc<-( ( -C_tcc*Stcox*Sccox*Htcox*Hccox )*( C_c ) + ( C_tc*C_cc*Stcox*Sccox*Htcox*Hccox ) )/(C_c^2)
  ddlnC<-( ( -C_tc*Stcox*Sccox*Htcox*Hccox )*( Co ) + ( C_c*C_t*Stcox*Sccox*Htcox*Hccox ) )/(Co^2)
  Ibp <- matrix(0, p, p)
  for(i in 1:p) # i repesents the row number, and j represents the column number of each hessian matrix
  {
    zi<-cova[,i]
    for(j in 1:p)
    {
      zj<-cova[,j]
      ddlnCtzz<-ddlnCt*zi*zj
      ddlnCczz<-ddlnCc*zi*zj
      ddlnCzz<-ddlnC*zi*zj
      Ibp[i,j]<- -sum( ddlnCtzz[del==1] )-sum( ddlnCczz[eta==1] )-sum( ddlnCzz[del==0 & eta==0] )
    }
  }
  return(Ibp)
}

# Hessian fucntion respect to phi
Hess_phi<-function(p, Co, dC, dC3, cova, Hccox, Sccox, del, eta)
{
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  ddHc<-Hccox
  ddlnCt<-( ( -( C_tcc )*( Sccox*Hccox )^2 - ( C_tc*Sccox )*( Hccox )^2 + ( C_tc*Sccox*Hccox ) )*( C_t ) + ( C_tc*Sccox*Hccox )^2 )/(C_t^2)
  ddlnCc<-( ( -( C_ccc )*( Sccox*Hccox )^2 - ( C_cc*Sccox )*( Hccox )^2 + ( C_cc*Sccox*Hccox ) )*( C_c ) + ( C_cc*Sccox*Hccox )^2 )/(C_c^2)
  ddlnC<-( ( -( C_cc )*( Sccox*Hccox )^2 - ( C_c*Sccox )*( Hccox )^2 + ( C_c*Sccox*Hccox ) )*( Co ) + ( C_c*Sccox*Hccox )^2 )/(Co^2)
  Ipp <- matrix(0, p, p)
  for(i in 1:p) # i repesents the row number, and j represents the column number of each hessian matrix
  {
    zi<-cova[,i]
    for(j in 1:p)
    {
      zj<-cova[,j]
      ddHczz<-ddHc*zi*zj
      ddlnCtzz<-ddlnCt*zi*zj
      ddlnCczz<-ddlnCc*zi*zj
      ddlnCzz<-ddlnC*zi*zj
      Ipp[i,j]<- -sum( ddHczz[eta==1] )-sum( ddlnCtzz[del==1] )-sum( ddlnCczz[eta==1] )-sum( ddlnCzz[del==0 & eta==0] )
    }
  }
  return(Ipp)
}
#(psix, Psix, bslht, h0t, eregt, avi, ID, del, eta, binwv, dC, Co, smpart, Rt)
# the denominator of the step functions
denomtreg=function(Rt, psix, Psix, bslht, dC, Co, Scoxt, eregt, del, eta, smpart)
{
  n<-dim(psix)[1];m=dim(psix)[2]
  J<-Rt%*%bslht
  PJ<-J
  PJ[J<0]<-0
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  posC_tt<-C_tt
  posC_tt[C_tt<0]<-0
  posHi1<-del*posC_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  SiT<-Scoxt
  deno<-colSums( matrix( ( ( del+posHi1*SiT )*eregt ),n,m )*Psix ) + smpart*PJ
  return(deno)
}

denomcreg=function(Rc, psix, Psix, bslhc, dC, Co, Scoxc, eregc, del, eta, smparc)
{
  n<-dim(psix)[1];m=dim(psix)[2]
  J<-Rc%*%bslhc
  PJ<-J
  PJ[J<0]<-0
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  posC_cc<-C_cc
  posC_cc[C_cc<0]<-0
  posHi2<-eta*posC_cc/C_c+del*C_tc/C_t+(1-del-eta)*C_c/Co
  SiC<-Scoxc
  deno<-colSums( matrix( ( ( eta+posHi2*SiC )*eregc ),n,m )*Psix ) + smparc*PJ
  return(deno)
}

# Hessian matrix respect to theta
Hess_theta<-function(Co, dC, dC3, psix, Psix, del, eta, ht0, eregt, Scoxt, smpart, Rt)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiT<-Scoxt
  Hi1<-del*C_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  dHi1dSiT<-del*( C_ttt*C_t - C_tt^2 )/C_t^2+eta*( C_tct*C_c - C_tc^2 )/C_c^2+(1-del-eta)*( C_tt*Co - C_t^2 )/Co^2
  dSiTdth<--matrix( SiT*eregt,n,m )*Psix
  Ithth <- t(matrix( -del/ht0^2,n,m )*psix)%*%psix - t( matrix( dHi1dSiT*SiT,n,m )*dSiTdth + matrix( Hi1,n,m )*dSiTdth )%*%( matrix( eregt,n,m )*Psix ) - smpart*Rt
  return(Ithth)
}

# Hessian matrix respect to gamma
Hess_gamma<-function(Co, dC, dC3, psix, Psix, del, eta, hc0, eregc, Scoxc, smparc, Rc)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiC<-Scoxc
  Hi2<-eta*C_cc/C_c+del*C_tc/C_t+(1-del-eta)*C_c/Co
  dHi2dSiC<-eta*( C_ccc*C_c - C_cc^2 )/C_c^2+del*( C_tcc*C_t - C_tc^2 )/C_t^2+(1-del-eta)*( C_cc*Co - C_c^2 )/Co^2
  dSiCdga<--matrix( SiC*eregc,n,m )*Psix
  Igaga <- t(matrix( -eta/hc0^2,n,m )*psix)%*%psix - t( matrix( dHi2dSiC*SiC,n,m )*dSiCdga + matrix( Hi2,n,m )*dSiCdga )%*%( matrix( eregc,n,m )*Psix ) - smparc*Rc
  return(Igaga)
}

# Hessian matrix respect to theta and gamma
Hess_theta_gamma<-function(Co, dC, dC3, psix, Psix, del, eta, eregt, eregc, Scoxt, Scoxc)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiT<-Scoxt;SiC=Scoxc
  Hi1<-del*C_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  dHi1dSiC<-del*( C_tct*C_t - C_tt*C_tc )/C_t^2+eta*( C_tcc*C_c - C_tc*C_cc )/C_c^2+(1-del-eta)*( C_tc*Co - C_t*C_c )/Co^2
  dSiCdga<--matrix( SiC*eregc,n,m )*Psix
  Ithga <- -t( matrix( dHi1dSiC*SiT,n,m )*dSiCdga )%*%( matrix( eregt,n,m )*Psix )
  return(Ithga)
}

# Hessian matrix respect to beta and theta
Hess_beta_theta<-function(Co, dC, dC3, psix, Psix, cova, del, eta, eregt, Hcoxt, Scoxt)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  p<-dim(cova)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiT<-Scoxt;
  LamiT<-Hcoxt
  Hi1<-del*C_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  dHi1dSiT<-del*( C_ttt*C_t - C_tt^2 )/C_t^2+eta*( C_tct*C_c - C_tc^2 )/C_c^2+(1-del-eta)*( C_tt*Co - C_t^2 )/Co^2
  dSiTdb<--matrix( SiT*LamiT,n,p )*cova
  Ibth<--t(matrix( dHi1dSiT*SiT,n,p )*dSiTdb + matrix( Hi1,n,p )*dSiTdb + matrix( del+Hi1*SiT,n,p )*cova)%*%( matrix( eregt,n,m )*Psix )
  return(Ibth)
}

# Hessian matrix respect to phi and theta
Hess_phi_theta<-function(Co, dC, dC3, psix, Psix, cova, del, eta, eregt, Hcoxc, Scoxt, Scoxc)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  p<-dim(cova)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiT<-Scoxt; SiC=Scoxc; LamiC=Hcoxc
  Hi1<-del*C_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  dHi1dSiC<-del*( C_tct*C_t - C_tt*C_tc )/C_t^2+eta*( C_tcc*C_c - C_tc*C_cc )/C_c^2+(1-del-eta)*( C_tc*Co - C_t*C_c )/Co^2
  dSiCdp<--matrix( SiC*LamiC,n,p )*cova
  Ipth<--t(matrix( dHi1dSiC*SiT,n,p )*dSiCdp)%*%( matrix( eregt,n,m )*Psix )
  return(Ipth)
}

# Hessian matrix respect to beta and gamma
Hess_beta_gamma<-function(Co, dC, dC3, psix, Psix, cova, del, eta, eregc, Hcoxt, Scoxt, Scoxc)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  p<-dim(cova)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiT<-Scoxt; SiC=Scoxc; LamiT=Hcoxt
  Hi2<-eta*C_cc/C_c+del*C_tc/C_t+(1-del-eta)*C_c/Co
  dHi2dSiT<-del*( C_tct*C_t - C_tt*C_tc )/C_t^2+eta*( C_tcc*C_c - C_tc*C_cc )/C_c^2+(1-del-eta)*( C_tc*Co - C_t*C_c )/Co^2
  dSiTdb<--matrix( SiT*LamiT,n,p )*cova
  Ibga<--t(matrix( dHi2dSiT*SiC,n,p )*dSiTdb)%*%( matrix( eregc,n,m )*Psix )
  return(Ibga)
}

# Hessian matrix respect to phi and gamma
Hess_phi_gamma<-function(Co, dC, dC3, psix, Psix, cova, del, eta, eregc, Hcoxc, Scoxc)
{
  n<-dim(psix)[1]
  m<-dim(psix)[2]
  p<-dim(cova)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_tt<-dC[,4]
  C_cc<-dC[,5]
  C_ttt<-dC3[,1]
  C_ccc<-dC3[,2]
  C_tct<-dC3[,3]
  C_tcc<-dC3[,4]
  SiC<-Scoxc; LamiC=Hcoxc
  Hi2<-eta*C_cc/C_c+del*C_tc/C_t+(1-del-eta)*C_c/Co
  dHi2dSiC<-eta*( C_ccc*C_c - C_cc^2 )/C_c^2+del*( C_tcc*C_t - C_tc^2 )/C_t^2+(1-del-eta)*( C_cc*Co - C_c^2 )/Co^2
  dSiCdp<--matrix( SiC*LamiC,n,p )*cova
  Ipga<--t(matrix( dHi2dSiC*SiC,n,p )*dSiCdp + matrix( Hi2,n,p )*dSiCdp + matrix( eta+Hi2*SiC,n,p )*cova)%*%( matrix( eregc,n,m )*Psix )
  return(Ipga)
}

# Hessian matrix respect to the MPL estimates
hesscoxphcopmpl<-function(p, Co, dC, dC3, cova, ht0, hc0, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, psix, Psix, del, eta, smpart, Rt, smparc, Rc)
{
  hess_b<-Hess_beta(p, Co, dC, dC3, cova, Hcoxt, Scoxt, del, eta)
  hess_ph<-Hess_phi(p, Co, dC, dC3, cova, Hcoxc, Scoxc, del, eta)
  hess_ph_b<-Hess_phi_beta(p, Co, dC, dC3, cova, Hcoxt, Hcoxc, Scoxt, Scoxc, del, eta)
  hess_th<-Hess_theta(Co, dC, dC3, psix, Psix, del, eta, ht0, eregt, Scoxt, smpart, Rt)
  #(Co, dC, dC3, psix, Psix, del, eta, ht01, eregt, Stcox01, 100, Rt)
  hess_ga<-Hess_gamma(Co, dC, dC3, psix, Psix, del, eta, hc0, eregc, Scoxc, smparc, Rc)
  #(Co, dC, dC3, psix, Psix, del, eta, hc01, eregc, Sccox01, 0, Rc)
  hess_b_th<-Hess_beta_theta(Co, dC, dC3, psix, Psix, cova, del, eta, eregt, Hcoxt, Scoxt)
  #(Co, dC, dC3, psix, Psix, cova, del, eta, eregt, Htcox01, Stcox01)
  hess_b_ga<-Hess_beta_gamma(Co, dC, dC3, psix, Psix, cova, del, eta, eregc, Hcoxt, Scoxt, Scoxc)
  #(Co, dC, dC3, psix, Psix, cova, del, eta, eregc, Htcox01, Stcox01, Sccox01)
  hess_ph_th<-Hess_phi_theta(Co, dC, dC3, psix, Psix, cova, del, eta, eregt, Hcoxc, Scoxt, Scoxc)
  #(Co, dC, dC3, psix, Psix, cova, del, eta, eregt, Hccox01, Stcox01, Sccox01)
  hess_ph_ga<-Hess_phi_gamma(Co, dC, dC3, psix, Psix, cova, del, eta, eregc, Hcoxc, Scoxc)
  #(Co, dC, dC3, psix, Psix, cova, del, eta, eregc, Hccox01, Sccox01)
  hess_th_ga<-Hess_theta_gamma(Co, dC, dC3, psix, Psix, del, eta, eregt, eregc, Scoxt, Scoxc)
  #(Co, dC, dC3, psix, Psix, del, eta, eregt, eregc, Stcox01, Sccox01)
  hess_b_ph<-t(hess_ph_b)
  hess_th_b<-t(hess_b_th)
  hess_th_ph<-t(hess_ph_th)
  hess_ga_b<-t(hess_b_ga)
  hess_ga_ph<-t(hess_ph_ga)
  hess_ga_th<-t(hess_th_ga)

  hess_b_ph_th_ga<-rbind(cbind(hess_b, hess_b_ph, hess_b_th, hess_b_ga), cbind(hess_ph_b, hess_ph, hess_ph_th, hess_ph_ga), cbind(hess_th_b, hess_th_ph, hess_th, hess_th_ga), cbind(hess_ga_b, hess_ga_ph, hess_ga_th, hess_ga))
  return(hess_b_ph_th_ga)
}

# score function respect to beta for individual 'i'
dplcox_dbeta_i<-function(p, cova, Htcox, avi, del, eta, dC, Co)
{
  n<-length(del)
  C_tt<-dC[,4]
  C_tc<-dC[,3]
  C_t<-dC[,1]
  C_c<-dC[,2]
  Htcoxz<-(Htcox%*%t(rep(1,p)))*cova
  Ctta<-((C_tt*avi)%*%t(rep(1,p)))*Htcoxz/(C_t%*%t(rep(1,p)))
  Ccta<-((C_tc*avi)%*%t(rep(1,p)))*Htcoxz/(C_c%*%t(rep(1,p)))
  Cta<-((C_t*avi)%*%t(rep(1,p)))*Htcoxz/(Co%*%t(rep(1,p)))
  delp<-del%*%t(rep(1,p))
  etap<-eta%*%t(rep(1,p))
  Ubeta_i<-cova*delp-Htcoxz*delp-Ctta*delp-Ccta*etap-Cta*(matrix(1, n, p)-delp)*(matrix(1, n, p)-etap)
  return(Ubeta_i)
}

# score function respect to phi for individual 'i'
dplcox_dphi_i<-function(p, cova, Hccox, bvi, del, eta, dC, Co)
{
  n<-length(del)
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_cc<-dC[,5]
  Hccoxz<-((Hccox)%*%t( rep(1,p) ) )*cova
  Ctcb<-((C_tc*bvi)%*%t( rep(1,p) ))*Hccoxz/(C_t%*%t(rep(1,p)))
  Cccb<-((C_cc*bvi)%*%t(rep(1,p)))*Hccoxz/(C_c%*%t(rep(1,p)))
  Ccb<-((C_c*bvi)%*%t(rep(1,p)))*Hccoxz/(Co%*%t(rep(1,p)))
  delp<-del%*%t(rep(1,p))
  etap<-eta%*%t(rep(1,p))
  Uphi_i<-cova*etap-Hccoxz*etap-Ctcb*delp-Cccb*etap-Ccb*(matrix(1, n, p)-delp)*(matrix(1, n, p)-etap)
  return(Uphi_i)
}

# score function respect to theta for individual 'i'
dplcox_dtheta_i<-function(psix, Psix, bslht, h0t, eregt, avi, del, eta, dC, Co, smpart, Rt)
{
  n<-dim(psix)[1];m=dim(psix)[2]
  C_tt<-dC[,4] #d( dC(u, v; alpha)/du )du
  C_tc<-dC[,3] #d( dC(u, v; alpha)/du )dv, where u=S_T(x_i), v=S_C(x_i)
  C_t<-dC[,1]
  C_c<-dC[,2]
  Hi1<-del*C_tt/C_t+eta*C_tc/C_c+(1-del-eta)*C_t/Co
  SiT<-avi
  Utheta_i <- matrix( ( del/h0t ),n,m )*psix - matrix( ( ( del+Hi1*SiT )*eregt ),n,m )*Psix - matrix(1,n,1)%*%t(smpart/n*Rt%*%bslht)
  return(Utheta_i)
}

# score function respect to gamma for individual 'i'
dplcox_dgamma_i<-function(psix, Psix, bslhc, h0c, eregc, bvi, del, eta, dC, Co, smparc, Rc)
{
  n<-dim(psix)[1];m=dim(psix)[2]
  C_t<-dC[,1]
  C_c<-dC[,2]
  C_tc<-dC[,3]
  C_cc<-dC[,5]
  Hi2<-eta*C_cc/C_c+del*C_tc/C_t+(1-del-eta)*C_c/Co
  SiC<-bvi
  Ugamm_i<-matrix( ( eta/h0c ),n,m )*psix - matrix( ( ( eta+Hi2*SiC )*eregc ),n,m )*Psix - matrix(1,n,1)%*%t(smparc/n*Rc%*%bslhc)
  return(Ugamm_i)
}

# Asympototic variance of the MPL estimates for all non-zero bins
asycoxphdepcencopmpl<-function(Co, dC, dC3, cova, psix, Psix, bslht, bslhc, Uth, Uga, ht0, hc0, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, del, eta, smpart, Rt, smparc, Rc, eps, mid, ac.theta, ac.gamma, ac.Utheta, ac.Ugamma)
{
  n<-dim(psix)[1];m=dim(psix)[2];p=dim(cova)[2]
  hess_b_ph_th_ga_mpl <- -hesscoxphcopmpl(p, Co, dC, dC3, cova, ht0, hc0, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, psix, Psix, del, eta, smpart, Rt, smparc, Rc)
  diag(hess_b_ph_th_ga_mpl) <- diag(hess_b_ph_th_ga_mpl)+eps
  if( identical(cova, matrix(0, n, 1)) ){noX = TRUE}else{noX = FALSE}
  pos <- c( if(noX){rep(FALSE, 2*p)}else{rep(TRUE, 2*p)}, c(bslht>=ac.theta, bslhc>=ac.gamma) | c(Uth>=ac.Utheta, Uga>=ac.Ugamma) )

  invhess_b_ph_th_ga_mpl <- matrix(0, 2*(m+p), 2*(m+p))
  invhess_b_ph_th_ga_mpl[pos,pos] <- solve(hess_b_ph_th_ga_mpl[pos,pos])

  if(mid==1) #mid: type of the sandwich formula
  {
    hess_b_ph_th_ga_mle <- -hesscoxphcopmpl(p, Co, dC, dC3, cova, ht0, hc0, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, psix, Psix, del, eta, 0, Rt, 0, Rc)
  }
  else if(mid==2)
  {
    Ubet_i <- dplcox_dbeta_i( p, cova, Hcoxt, Scoxt, del, eta, dC, Co )
    Uph_i <- dplcox_dphi_i( p, cova, Hcoxc, Scoxc, del, eta, dC, Co )
    Uth_i <- dplcox_dtheta_i( psix, Psix, bslht, ht0, eregt, Scoxt, del, eta, dC, Co, smpart, Rt )
    #(psix, Psix, h0t0, ht01, eregt, Stcox01, del, eta, dC, Co, 100, Rt)
    Uga_i <- dplcox_dgamma_i( psix, Psix, bslhc, hc0, eregc, Scoxc, del, eta, dC, Co, smparc, Rc )
    #(psix, Psix, h0c0, hc01, eregc, Sccox01, del, eta, dC, Co, 0, Rc)
    Uth_i <- Uth_i[,c(bslht>=ac.theta, bslhc>=ac.gamma) | c(Uth>=ac.Utheta, Uga>=ac.Ugamma)]
    Uga_i <- Uga_i[,c(bslht>=ac.theta, bslhc>=ac.gamma) | c(Uth>=ac.Utheta, Uga>=ac.Ugamma)]
    U_i <- cbind(Ubet_i, Uph_i, Uth_i, Uga_i)
    hess_b_ph_th_ga_mle <- t(U_i)%*%U_i
  }
  else
  {
    hess_b_ph_th_ga_mle <- -hesscoxphcopmpl(p, Co, dC, dC3, cova, ht0, hc0, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, psix, Psix, del, eta, smpart, Rt, smparc, Rc)
  }
  asymcov <- invhess_b_ph_th_ga_mpl%*%hess_b_ph_th_ga_mle%*%invhess_b_ph_th_ga_mpl
  diag( asymcov )[ diag( asymcov )<0 ] <- 0
  asymstd <- sqrt( diag( asymcov ) )
  asymstd_b <- asymstd[1:p]
  asymstd_ph <- asymstd[(p+1):(2*p)]
  asymstd_th <- asymstd[(2*p+1):(2*p+m)]
  asymstd_ga <- asymstd[(2*p+m+1):(2*p+2*m)]
  return(list(asympcov_est=asymcov, asympstd_beta=asymstd_b, asympstd_phi=asymstd_ph, asympstd_theta=asymstd_th, asympstd_gamma=asymstd_ga))
}




































































