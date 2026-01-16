#' Generalized Cross-Correlation Matrix
#'
#' Built the GCC similarity matrix between time series proposed in Alonso and Peña (2019).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param lag Selected lag for computing the GCC between the pairs of series.
#' Default value is computed inside the program.
#' @param model Model specification. When the value lag is unknown for the user,
#' the model specification is chosen between GARCH model or AR model. Default is ARMA model.
#' @param lag.set If lag is not specified and the user wants to use instead of lags from 1 to 'lag' a non consecutive set of lags they can be defined as lag.set = c(1, 4, 7).
#'
#' @return A list containing:
#' \itemize{
#' \item DM - A matrix object with the distance matrix.
#' \item k_GCC - The lag used to calculate GCC measure.
#' }
#'
#' @importFrom fGarch garchFit
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- GCCmatrix(TaiwanAirBox032017[,1:3])
#'
#' @export
#'
#' @references Alonso, A. M. and Peña, D. (2019). Clustering time series by linear
#' dependency. \emph{Statistics and Computing}, 29(4):655–676.
#'
GCCmatrix <- function(x, lag, model, lag.set){

  zData <- x

  if(missing(lag) & missing(lag.set)){

    if(missing(model)){

      setAR <- function(x){
        N <- length(x)

        orderMax <- min(10, N/10)
        spfinal.bic <- Inf
        spfinal.order <- c(0,0,0)
        for(i in 0:orderMax){
          model0 <- arima(x, order=c(i, 0, 0))
          spcurrent.aic <- AIC(model0)
          spcurrent.bic <- BIC(model0)
          if (spcurrent.bic < spfinal.bic) {
            spfinal.bic <- spcurrent.bic
            spfinal.order <- c(i, 0, 0)
          }
        }
        arOrder <- spfinal.order[1]

        return(arOrder)
      }


      PP <- apply(zData, 2, setAR)
      kSup <- max(PP)

    } else{
      orderArch <- function(x){
        N <- length(x)
        orderMax <- min(5, N/10)
        aic <- NULL
        bic <- NULL
        for(i in 1:orderMax){
          model1 <- fGarch::garchFit(substitute(~garch(i,0),list(i=i)), data = x)
          aic <- c(aic, (2*(i+2) + 2*model1@fit$value)/1000)
          bic <- c(bic, (log(1000)*(i+2) + 2*model1@fit$value)/1000)
        }
        return(which.min(bic))
      }

      PP <- apply(sqrt(zData), 2, orderArch)
      kSup <- max(PP)
    }

    kOp <- data.frame()
    kOp1 <- 0
    kinf <- 0
    for(jj in 1:(ncol(zData)-1)){
      for (ii in (jj+1):ncol(zData)){
        if(kOp1 == kSup){
          break
        }
        kOp1 <- c(kOptim(zData[, jj], zData[, ii], kSup, kinf))

        kOp <- rbind(kOp, data.frame(i = jj, j = ii, kOp1))
        kinf <- kOp1

      }
    }


    if(nrow(kOp) != 0){ kDef <- max(kOp$kOp1)
    } else kDef= kSup

  }
  if(missing(lag) & !missing(lag.set)){
    kDef <- max(lag.set)
  }
  if(!missing(lag)) kDef <- lag

  nSerie <- ncol(zData)
  DM <- diag(x = 0, nrow = nSerie, ncol = nSerie)

  for(ii in 1:nSerie){
    for(jj in ii:nSerie){
      if(missing(lag.set)){
        g <- GCC_sim2(zData[, ii], zData[, jj], kDef)
      } else{
        g <- GCC_sim2(zData[, ii], zData[, jj], lag.set = lag.set)
      }
      DM[ii, jj] <- 1 - g
      DM[jj, ii] <- 1 - g
    }
  }

  if(!is.null(colnames(zData))){
    colnames(DM) <- colnames(zData)
  } else{
    colnames(DM) <- 1:ncol(zData)
    rownames(DM) <- 1:ncol(zData)

  }
  sale <- list(DM = DM,   k_GCC = kDef)
  return(sale)
}

kOptim <- function(xData, yData, kMax, kinf){

  N <- length(xData)
  M_d <- matrix(nrow = N-kMax, ncol = 2*kMax+2)

  for(i in 1:(kMax+1)){
    M_d[, i]       <- xData[i:(N - kMax+i-1)]
    M_d[, (i + kMax + 1)] <- yData[i:(N - kMax+i-1)]
  }

  M_d <- data.frame( M_d)
  names(M_d) <- c(paste("x.lag", kMax:0, sep = ""),
                  paste("y.lag", kMax:0, sep = ""))

  bic1 <- data.frame()

  for (jj in kinf:(kMax)){
    b1 <- BIC(model <- lm(x.lag0 ~ . , data = M_d[, c((kMax+1-jj):(kMax+1),
                                                      (2*(kMax+1)-jj):(2*(kMax+1)))]))
    bic1 <- rbind(bic1, c(jj, b1))
  }

  names(bic1) <- c("k", "BIC")
  p1 <- bic1[which.min(bic1$BIC), "k"]

  if(p1 < kMax){
    bic2 <- data.frame()

    for (jj in kinf:(kMax)){
      b2 <- BIC(model <- lm(y.lag0 ~ . , data = M_d[, c((kMax+1-jj):(kMax+1),
                                                        (2*(kMax+1)-jj):(2*(kMax+1)))]))
      bic2 <- rbind(bic2, c(jj, b2))
    }

    names(bic2) <- c("k", "BIC")
    p2 <- bic1[which.min(bic2$BIC), "k"]
    p  <- max(p1, p2)

  }

  if(!exists("p")) p <- p1

  return(p)
}

GCC_sim2 <- function(xData, yData, k, lag.set ){

  if(missing(lag.set)){
    lag.set <- 0:k
  }
  if(missing(k)){
    k <- max(lag.set)
  }


  N <- length(xData)

  correla <- acf(cbind(xData, yData), lag.max = k, plot = FALSE)



  R_xx <- matrix(0, ncol = (k+1), nrow = (k+1))
  rownames(R_xx) <- 0:k
  colnames(R_xx) <- 0:k
  for(i in 1:(k+1)){
    j <- i-1
    if(i<=k ) R_xx[i, (i+1):(k+1) ] <-  correla$acf[ 2:(k+1-j), 1, 1]
    R_xx[i, 1:i ] <- correla$acf[sort(1:i, decreasing = TRUE), 1, 1]
  }

  R_yy <- matrix(0, ncol = (k+1), nrow = (k+1))
  rownames(R_yy) <- 0:k
  colnames(R_yy) <- 0:k

  for(i in 1:(k+1)){
    j <- i-1
    if(i<=k ) R_yy[i, (i+1):(k+1) ] <-  correla$acf[ 2:(k+1-j), 2, 2]
    R_yy[i, 1:i ] <- correla$acf[sort(1:i, decreasing = TRUE), 2, 2]
  }


  tC_xy <- matrix(0, ncol = (k+1), nrow = (k+1))

  for(i in 1:(k+1)){
    j <- i-1
    if(i<=k ) tC_xy[i, (i+1):(k+1) ] <-  correla$acf[ 2:(k+1-j), 2, 1]
    tC_xy[i, 1:i ] <- correla$acf[sort(1:i, decreasing = TRUE), 1, 2]
  }

  R_yx <- rbind(cbind(R_yy, tC_xy), cbind(t(tC_xy), R_xx))
  rownames(R_yx) <- c(0:k,0:k)
  colnames(R_yx) <- c(0:k,0:k)

  if(length(lag.set)==1){

    R_yx  <- R_yx[c(lag.set+1,(k+2+lag.set)), c(lag.set+1, (k+2+lag.set))]
    R_yy  <- R_yy[lag.set+1, lag.set+1]
    R_xx  <- R_xx[lag.set+1, lag.set+1]
    GCC <- 1 - det(R_yx)^(1/ (1 * (k+1)))  / (R_xx^(1/ (1 * (k+1))) * R_yy^(1/ (1 * (k+1))))


  } else  {

    R_yx  <- R_yx[c(lag.set+1, (max(lag.set)+2+lag.set)), c(lag.set+1, (max(lag.set)+2+lag.set))]
    R_yy  <- R_yy[lag.set+1, lag.set+1]
    R_xx  <- R_xx[lag.set+1, lag.set+1]

    GCC <- 1 - det(R_yx)^(1/ (1 * (k+1)))  / (det(R_xx)^(1/ (1 * (k+1))) * det(R_yy)^(1/ (1 * (k+1))))


  }

  return(GCC)

}
