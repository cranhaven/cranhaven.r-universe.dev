#' Function to use Monte Carlo strategy
#'
#' @param object as output of 'prediction_errors()' function
#' @param size as volume of time series used in Monte Carlo strategy
#' @param iteration as number of iterations models to be applied
#' @param fval as a flag to view forecasted values in each iteration (default: 0, don't view values)
#' @param figs as a flag to view plots for each iteration (default: 0, don't view plots)
#' @import forecast
#' @import PSF
#' @importFrom imputeTestbench rmse mae mape
#' @importFrom stats ts
#' @importFrom methods hasArg
#' @importFrom graphics plot
#' @return Error values with provided models in each iteration along with the mean values
#' @export
#' @examples
#' \dontrun{
#' library(forecast)
#' test3 <- function(data, nval){return(as.numeric(forecast(ets(data), h = nval)$mean))}
#' a <- prediction_errors(data = nottem,
#'     Method = c("test3(data, nval)"),
#'     MethodName = c("ETS"), append_ = 1)
#' monte_carlo(object = a1, size = 144, iteration = 10)
#' }

#===================
# Monte Carlo Errors
#===================
monte_carlo <- function(object, size, iteration, fval = 0, figs = 0){
  a <- object
  data <- a@parameters$data
  x <- a@parameters$nval # nval- number of values to be forecasted
  # size is the size of block to be selected from data
  # iteration is number of iteration in Monte Carlo

  ran <- sample(1:(length(data)-size), iteration, replace=FALSE)  # number of sample cases (Monte-Carlo)

  # x <- 24 # number of values to be predicted
  # size <- 360 # size of dataset for training

  err <- NULL
  xn <- NULL
  xfig <- NULL
  xx <- 0
  mylist=list()
  mylist1=list()
  p <- 1
  if(fval == 0){
    for(case in ran){
      train_d <- data[case:(case+size)]
      test_d <- data[(case+size+1):(case+size+24)]

      x <- prediction_errors(data = c(train_d, test_d), nval=a@parameters$nval, strats = a@parameters$Strategy,
                             Method = a@parameters$Method[-1], MethodName = a@parameters$MethodName[-1], append_ = "1")
      gg <- x@output$Error_Parameters[,-3]
      ff <- data.frame(t(gg))[1,]
      xx <- append(xx, case)
      if(is.null((err))){err <- ff}
      else{err <- rbind(err, ff)}
    }

    for(i in 1:length(err)){
      err[,i] <- as.numeric(as.character(err[,i]))
    }

    err <- rbind(err, c(apply(err[,1:length(err)], 2, mean)))
    rownames(err) <- c(xx[-1], 'Mean')

    return(err)
  } else{
    for(case in ran){
      train_d <- data[case:(case+size)]
      test_d <- data[(case+size+1):(case+size+24)]

      x <- prediction_errors(data = c(train_d, test_d), nval=a@parameters$nval, strats = a@parameters$Strategy,
                             Method = a@parameters$Method[-1], MethodName = a@parameters$MethodName[-1], append_ = "1")
      gg <- x@output$Error_Parameters[,-3]
      ff <- data.frame(t(gg))[1,]
      xx <- append(xx, case)
      if(is.null((err))){err <- ff}
      else{err <- rbind(err, ff)}
      xn[[p]] <- x@output$Predicted_Values
      if(figs != 0){
        xfig[p] <- plot(x)$Predicted_Values
      }
      p <- p + 1
    }

    for(i in 1:length(err)){
      err[,i] <- as.numeric(as.character(err[,i]))
    }

    err <- rbind(err, c(apply(err[,1:length(err)], 2, mean)))
    rownames(err) <- c(xx[-1], 'Mean')


      d=list( mylist, mylist1)
      names(d)=c("Error_Parameters","Predicted_Values")
      d$Error_Parameters <- err
      d$Predicted_Values <- xn
    return(d)

  }


}

