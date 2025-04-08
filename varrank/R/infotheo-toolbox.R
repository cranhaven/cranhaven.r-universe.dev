###############################################################################
## infotheo-toolbox.R ---
## Author             : Gilles Kratzer
## Document created   : 07/09/2017
##                    : 08/09/2017
###############################################################################

##---------------------------------------------------------------------------------------------------------
## Discretization of an arbitrary large number of variables joint variables depending on their distribution
## Implemented discretization methods:
## -user defined
## -fd
## -doane
## -sqrt
## -sturges
## -rice
## -scott
## -cencov
## -kmeans
## -terrell-scott
##---------------------------------------------------------------------------------------------------------

discretization <- function(data.df = NULL, discretization.method = "cencov", frequency = FALSE){

  #tests:

  if(is.character(discretization.method)) {
    discretization.method <- tolower(discretization.method)


  discretization.method <- c("fd","doane","cencov","sturges","rice","scott","kmeans","terrell-scott")[pmatch(discretization.method,c("fd","doane","cencov","sturges","rice","scott","kmeans","terrell-scott"))]
  if ( (is.na(discretization.method)) || (length(discretization.method)>1) ){
    discretization.method <- "cencov"
    warning("Discretization method not recognised; set to 'cencov'")}
} else {
  if (!is.numeric(discretization.method) || discretization.method < 2 )
    stop("For equal binning, discretization.method (as numeric) >=2 is required.")

  #if(!(discretization.method %in% c("fd","doane","cencov","sturges","rice","scott","kmeans","terrell-scott") | is.numeric(discretization.method))){stop("Wrong definition of the discretization.method's parameter")}
}
  ##end of tests

  data.df <- as.data.frame(data.df)
  nobs <- nrow(data.df)
  nvar <- ncol(data.df)

  df.tab <- data.df

  ind <- sapply(data.df, is.numeric)

  ##===============================================================
  ## Discrete cutoff provided
  ##===============================================================

  if(is.numeric(discretization.method)){

    fun.discrete <- function(df){
      df.out <- as.factor(cut(as.numeric(df), breaks = discretization.method, include.lowest = TRUE))
      return(as.factor(df.out))
    }
    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.discrete)
    }

  ##===============================================================
  ## kmeans
  ##===============================================================

  if(discretization.method=="kmeans"){

    fun.kmeans <- function(df){
      wss <- (nobs-1)*sum(stats::var(df))
      j <- 2
      ratio <- 1

      while(ratio>.2 & j<nobs) {
        set.seed(42)
        wss1 <- (stats::kmeans(df,centers=j,iter.max = 100000,algorithm = "MacQueen")$tot.withinss)
        ratio <- abs((wss1-wss)/wss)
        wss <- wss1
        j <- j+1
      }
      set.seed(42)
      df.out <- stats::kmeans(x = df,centers = j,algorithm = "MacQueen",iter.max = 100000)$cluster
      return(as.factor(df.out))
    }

    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.kmeans)

    }

  ##===============================================================
  ##Freedman-Diaconis rule
  ##===============================================================

  if(discretization.method=="fd"){

    fun.fd <- function(df){
      cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(diff(range(df,na.rm = TRUE))/(2*stats::IQR(df) * nobs^(-1/3)))))
      df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))
    }

    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.fd)
    }

  ##===============================================================
  ##Doane’s formula
  ##===============================================================

  if(discretization.method=="doane"){

    fun.doane <- function(df){
      cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(1 + log2(nobs) + log2( 1 + (abs(skewness(df))/sqrt((6*(nobs)-2)/((nobs+1)*(nobs+2))))))))
      df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))
      return(df.out)
    }

    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.doane)
    }

  ##===============================================================
  ##cencov
  ##===============================================================

  if(discretization.method=="cencov"){

    fun.sqrt <- function(df){

      cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(nobs^(1/3))))

      df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))

      return(df.out)

    }
    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.sqrt)
  }

  ##===============================================================
  ##Sturges
  ##===============================================================

  if(discretization.method=="sturges"){

    fun.sturges <- function(df){
    cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(log(x = nobs,base = 2))))

    df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))

    return(df.out)

    }

    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.sturges)
}

  ##===============================================================
  ##Rice Rule
  ##===============================================================

  if(discretization.method=="rice"){

    fun.rice <- function(df){
    cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(2*nobs^(1/3))))

    df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))

    return(df.out)
    }
    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.rice)

}

  ##===============================================================
  ##Scott rule
  ##===============================================================

  if(discretization.method=="scott"){

    fun.scott <- function(df){
    cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(diff(range(df))/(3.5 * sd(df) / nobs^(1/3)))))
    df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))

    return(df.out)
    }
    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.scott)
  }


  ##===============================================================
  ##Terrell-Scott
  ##===============================================================

  if(discretization.method=="terrell-scott"){

    fun.terrell.scott <- function(df){
      cut.index <- seq(from = range(df,na.rm = TRUE)[1], to = range(df,na.rm = TRUE)[2], length.out = ceiling(abs(2*nobs)^(1/3)))
      df.out <- as.factor(cut(as.numeric(df), breaks = cut.index, include.lowest = TRUE))

      return(df.out)
    }
    df.tab[ind] <- lapply(X = data.df[ind], FUN = fun.terrell.scott)
  }

  df.tab <- as.data.frame(df.tab)
  names(df.tab) <- names(data.df)

  if(frequency){
    return(list(table=table(df.tab),df.discr=df.tab))
  }else{
    return(df.tab)
  }
}

##-------------------------------------------------------------------------
## Shanon Entropy
##-------------------------------------------------------------------------

entropy.data <- function(freqs.table){

  #normalization
  freqs.joint <- prop.table(freqs.table)

  #computing log part
  log.part <- ifelse(freqs.joint > 0, log(freqs.joint,base = 2), 0)

  #computing entropy
  entropy.out <- sum(freqs.joint * log.part)

  return(-entropy.out)

}

##------------------------------------------------------------------------------------
## Mutual Information
## Function that returns Mutual Information of a possibly discretized set of variables
##------------------------------------------------------------------------------------

mi.data <- function(X,Y,discretization.method=NULL,k=NULL){

  ##H(X)+H(Y)-H(X,Y)
  if(!is.null(discretization.method)){
    Hx <- entropy.data(discretization(data.df = X,discretization.method = discretization.method,frequency = TRUE)[[1]])
    Hy <- entropy.data(discretization(data.df = Y,discretization.method = discretization.method,frequency = TRUE)[[1]])
    Hxy <- entropy.data(discretization(data.df = cbind(X,Y),discretization.method = discretization.method,frequency = TRUE)[[1]])
    mi <- Hx+Hy-Hxy
  }
  if(!is.null(k)){
    mi <- FNN::mutinfo(X = X,Y = Y,k = k)
  }
  if(is.null(discretization.method) & is.null(k)){mi <- NA}

  return(mi)
}

##------------------------------------------------------------------------------------
## Mutual Information
## Function that returns Mutual Information of a discretized set of variables (optimized for discrete RV)
##------------------------------------------------------------------------------------


mi.data.discr <- function(X,Y){

  ##H(X)+H(Y)-H(X,Y)
    Hx <- entropy.data(freqs.table = table(X))
    Hy <- entropy.data(freqs.table = table(Y))
    Hxy <- entropy.data(freqs.table = table(cbind(as.data.frame(X),as.data.frame(Y))))
    mi <- Hx+Hy-Hxy
  return(mi)
}

##------------------------------------------------------------------------------------
## Table function for handling more than 2^31 elements
## Function that returns optimized table
##------------------------------------------------------------------------------------

# table.varrank <- function(x){
#
# }
##EOF
