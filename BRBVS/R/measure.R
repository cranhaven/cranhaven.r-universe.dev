#' Measure function
#'
#' This function calculates the importance measures for covariates related to survival functions using the specified copula and margins. It also provides the values of the Hessian matrix for each covariate.
#'
#' @param xs A data frame representing the b-th bootstrap sample of the data matrix $X=(X_1,\dots, X_p)$.
#' @param ys A data frame representing the b-th bootstrap sample of the matrix y containing the time to events and the censoring variables.
#' @param copula A character string specifying the type of copula employed in the algorithm. Default is 'C0'.
#' @param margins A character vector specifying the type of margin employed in the algorithm. Default is c('PH', 'PO').
#' @param metric Character, specifies the metric used for ranking the variables.
#'        Must be one of 'CE', 'FIM', 'Abs'. Default is 'FIM'.
#'
#' @return A list containing the following components:
#'   \itemize{
#'     \item \code{RankBeta1E}: Sorted importance measures for covariates related to the first survival function.
#'     \item \code{RankBeta2E}: Sorted importance measures for covariates related to the second survival function.
#'     \item '\code{HesVel.rank}: Values of the Hessian matrix for each covariate.
#'   }
#'
#' @import GJRM
#' @import utils
#' @noRd
#'
measure=function(xs,ys, copula, margins,  metric){




  #################################################
  ## Input
  #################################################
  # xs  = b-th bootstrap sample of x
  # ys = b-th bootstrap smaple of y
  # copula = type of copula employed in the algorithm
  # margins = type of margin employed in the algorithm

  #################################################
  ## Output
  #################################################

  # This function returns the covariates sorted by their level of importance based on the chosen metric.

  #library(GJRM)

  # Input validation

  x.d<- dim(xs)[2]
  stime.p <- data.frame(matrix(NA,ncol=4, nrow=dim(xs)[2]))
  colnames(stime.p) <- c('Beta1','Covariate1E','Beta2','Covariate2E')
  #stime.p2 <- data.frame(matrix(NA,ncol=4, nrow=dim(xs)[2]))
  #colnames(stime.p2) <- c('ABSBeta1','Covariate1E','ABSBeta2','Covariate2E')


  TotCov <- 1:dim(xs)[2]

  dataSim = as.data.frame(cbind(ys,xs))
  dataSim$cens = as.factor(dataSim$cens)

  cens1 = as.factor(substr(as.character(dataSim$cens), start = 1, stop = 1))
  cens2 = as.factor(substr(as.character(dataSim$cens), start = 2, stop = 2))

  dataSim$cens1 = cens1
  dataSim$cens2 = cens2

  #pb = utils::txtProgressBar(min = 0, max = dim(xs)[2], initial = 0, style = 3)

  HessinaValues <- matrix(NA, ncol=2, nrow=dim(xs)[2]) #just to store the value of hessian matrix
  #init <- numeric(dim(xs)[2])
  #end <- numeric(dim(xs)[2])

  for(ss in 1:dim(xs)[2]){

    #init[ss] <- Sys.time()

    # this list have to be redefined every iteration
    f.l <- list(  stats::as.formula( noquote(paste('t11 ~ s(t11, bs = "mpi")', "+", paste('z',TotCov[ss],
                                                                                   sep = '')))),
                  stats::as.formula( noquote(paste(' t21 ~ s(t21, bs = "mpi")', "+", paste('z',TotCov[ss],
                                                                                    sep = '')))),

                  stats::as.formula( noquote(paste( "~", 1)))
 )
#    f.l <- list(  stats::as.formula( noquote(paste('t11 ~ s(t11, bs = "mpi")', "+", ColNamesxs[ss]))),
#                  stats::as.formula( noquote(paste(' t21 ~ s(t21, bs = "mpi")', "+", ColNamesxs[ss]))),
#
#                  stats::as.formula( noquote(paste( "~", 1)))
#
#    )


    if ('I' %in% levels(cens1) || 'I' %in% levels(cens2)) {
    out<-try(GJRM::gjrm(f.l, data = dataSim, surv = TRUE,
                  copula = copula, margins = margins,
                  cens1 = cens1, cens2 = cens2, model = "B",
                  upperBt1 = 't12', upperBt2 = 't22') , silent=TRUE)
    } else{
      out<-try(GJRM::gjrm(f.l, data = dataSim, surv = TRUE,
                          copula = copula, margins = margins,
                          cens1 = cens1, cens2 = cens2, model = "B") , silent=TRUE)
}

    # some diagnostic checks
    if(class(out)[1] == "try-error"  )next
    if( max(abs(out$fit$gradient)) > 10 ) next
    if(!det(out$fit$hessian)>=0) next
    if(!sum(eigen(out$fit$hessian)$values)>1) next

    if(metric =='Abs'){

      stime.p[ss,1] <- abs(out$coefficients[2:2])
      stime.p[ss,2] <- names(out$coefficients[2:2])

      stime.p[ss,3] <-abs(out$coefficients[13:13])
      stime.p[ss,4] <- names(out$coefficients[13:13])

    }else if(metric =='CE'){

      stime.p[ss,1] <- copent::copent(cbind(out$fit$eta1,xs[,ss]))
      stime.p[ss,2] <- names(out$coefficients[2:2])

      stime.p[ss,3] <-copent::copent(cbind(out$fit$eta2,xs[,ss]))
      stime.p[ss,4] <- names(out$coefficients[13:13])

    }else{


      stime.p[ss,1] <- out$coefficients[2:2]^2*out$fit$hessian[2,2]
      stime.p[ss,2] <- names(out$coefficients[2:2])

      stime.p[ss,3] <-out$coefficients[13:13]^2*out$fit$hessian[13,13]
      stime.p[ss,4] <- names(out$coefficients[13:13])
    }


    HessinaValues[ss,]<- c(out$fit$hessian[2,2], out$fit$hessian[13,13])

    #end[ss] <- Sys.time()
    #these are saved to make a comparison
    # this does not need to be included in the GJRM function
    #setTxtProgressBar(pb, ss)
    #time <- round(lubridate::seconds_to_period(sum(end - init)), 0)

    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    #est <- dim(xs)[2] * (mean(end[end != 0] - init[init != 0])) - time
    #remainining <- round(lubridate::seconds_to_period(est), 0)

    #cat(paste(" // Execution time:", time,
    #          " // Estimated time remaining:", remainining), "")



  }

  OrderedList <- list(
    RankBeta1E=stime.p[order(stime.p[,'Beta1'], decreasing=T),c(which(names(stime.p)=='Beta1'): which(names(stime.p)=='Covariate1E'))],
    RankBeta2E=stime.p[order(stime.p[,'Beta2'], decreasing=T),c(which(names(stime.p)=='Beta2'): which(names(stime.p)=='Covariate2E'))],
    HesVel.rank=HessinaValues
  )

  return(OrderedList)
}
