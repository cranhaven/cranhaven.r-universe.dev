#' Collects All Models Specified by "arimaSpec"
#'
#' Collects all models Specified by "arimaSpec".
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param maxorder Maximum order of \eqn{(p,d,q)} where \eqn{p} is the AR order, \eqn{d} the degree of differencing,
#' and \eqn{q} the MA order. Default value is (5,1,3).
#' @param criterion Information criterion used for model selection. Either AIC or BIC.
#' Default is "bic".
#' @param method Estimation method. See the arima command in R. Possible values are "CSS-ML", "ML", and "CSS".
#' Default is "CSS".
#'
#' @return A list containing:
#' \itemize{
#'    \item Order - Orders \eqn{(p, d, q)} of each series. A matrix of (ncol(x),3). The three columns are "p", "d", "q".
#'    \item Mean - A logical vector indicating whether each series needs a constant (or mean).
#'    \item M1 - A matrix with three columns (p, 0, q). The number of rows is the number of stationary time series. M1 is NULL if there is no stationary series.
#'    \item M2 - A matrix with three columns (p, 1, q). The number of rows is the number of first-differenced series. M2 is NULL if there is no first-differenced series.
#'    \item M3 - A matrix with three columns (p, 2, q). The number of rows is the number of 2nd-differenced series. M3 is NULL if there is no 2nd-differenced series.
#'    \item data - Time series.
#' }
#'
#' @export
#'
#' @examples
#' x <- matrix(rnorm(300, mean = 10, sd = 4), ncol = 3, nrow = 100)
#' summary <- SummaryModel(x)
"SummaryModel" <- function(x, maxorder = c(5,1,3), criterion = "bic", method = "CSS"){

  if(!is.matrix(x))x <- as.matrix(x)
  Order <- NULL; Mean <- NULL
  for (i in 1:ncol(x)){
    ######    cat("series: ",i,"\n")
    m1 <- arimaSpec(x[,i],maxorder=maxorder,output=FALSE,criterion=criterion,method=method)
    Order <- rbind(Order,m1$order)
    Mean <- c(Mean,m1$include.mean)
  }

  k <- ncol(x)
  colnames(Order) <- c("p","d","q")
  rownames(Order) <- paste("x",1:k,sep="")

  istat <- c(1:k)[Order[,2]==0]
  nsta <- length(istat)
  M1 <- NULL
  if(nsta > 0){
    message("Number of stationary series: ",nsta,"\n")
    M1 <- Order[istat,]
    if(nsta > 1){
      pmax <- max(M1[,1]); qmax <- max(M1[,3])
      Tbl1 <- matrix(0,(pmax+1),(qmax+1))
      for (i in 1:nsta){
        ii <- M1[i,1]+1; jj <- M1[i,3]+1
        Tbl1[ii,jj] <- Tbl1[ii,jj]+1
      }
      rownames(Tbl1) <- paste("p=",0:pmax,sep="")
      colnames(Tbl1) <- paste("q=",0:qmax,sep="")
      #print(Tbl1)
    }else{
      message("Order: ", M1,"\n")
    }
  }else{
    message("All series are non-stationary","\n")
  }

  idiff1 <- c(1:k)[Order[,2]==1]
  ndiff1 <- length(idiff1)
  M2 <- NULL
  if(ndiff1 > 0){
    message("Number of first-differenced series: ",ndiff1,"\n")
    M2 <- Order[idiff1,]
    if(ndiff1 > 1){
      pmax1 <- max(M2[,1]); qmax1 <- max(M2[,3])
      Tbl2 <- matrix(0,(pmax1+1),(qmax1+1))
      for (i in 1:ndiff1){
        ii <- M2[i,1]+1; jj <- M2[i,3]+1
        Tbl2[ii,jj] <- Tbl2[ii,jj]+1
      }
      rownames(Tbl2) <- paste("p=",0:pmax1,sep="")
      colnames(Tbl2) <- paste("q=",0:qmax1,sep="")
      message("d=1","\n")
      #print(Tbl2)
    }else{
      message("Order: ",M2,"\n")
    }
  }

  idiff2 <- c(1:k)[Order[,2]==2]
  ndiff2 <- length(idiff2)
  M3 <- NULL
  if(ndiff2 > 0){
    message("Number of 2nd-differenced series: ",ndiff2,"\n")
    M3 <- Order[idiff2,]
    if(ndiff2 > 1){
      pmax2 <- max(M3[,1]); qmax2 <- max(M3[,3])
      Tbl3 <- matrix(0,(pmax2+1),(qmax2+1))
      for (i in 1:ndiff2){
        ii <- M3[i,1]+1; jj <- M3[i,3]+1
        Tbl3[ii,jj] <- Tbl3[ii,jj]+1
      }
      rownames(Tbl3) <- paste("p=",0:pmax2,sep="")
      colnames(Tbl3) <- paste("q=",0:qmax2,sep="")
      message("d=2","\n")
      #print(Tbl3)
    }else{
      message("Order: ",M3,"\n")
    }
  }

  SummaryModel <- list(order=Order,Mean=Mean,M1=M1,M2=M2,M3=M3,data=x)
}
