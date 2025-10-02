#' Function to append new methods in the study
#'
#' @param object as output of 'prediction_errors()' function
#' @param Method as the list of locations of function for the proposed prediction method
#' @param MethodName as list of names for function for the proposed prediction method in order
#' @param ePara as type of error calculation (RMSE and MAE are default), add an error parameter of
#'         your choice in the following manner: ePara = c("errorparametername")
#'         where errorparametername is should be a source/function which returns desired error set
#' @param ePara_name as list of  names of error parameters passed in order
#' @importFrom stats ts
#' @importFrom methods hasArg
#' @return Returns error comparison for additional forecasting methods
#' @export
#' @examples
#' \dontrun{
#' library(forecast)
#' test3 <- function(data, nval){return(as.numeric(forecast(ets(data), h = nval)$mean))}
#' a <- prediction_errors(data = nottem)
#' b <- append_(object = a, Method = c("test3(data,nval)"), MethodName = c('ETS'))
#' choose_(object = a)
#' }

append_ = function(object, Method, MethodName, ePara, ePara_name)
{
  a <- object
  b <- NULL
  c1 <- NULL
  M1 = a@parameters$Method
  M2 = a@parameters$MethodName
  if(!(hasArg(MethodName)))
  {MethodName <- a@parameters$MethodName}

  if(!(hasArg(Method)))
  {Method <- a@parameters$Method}
  else
  {
    b <- prediction_errors(data = a@parameters$data,
                           nval = a@parameters$nval,
                           ePara = a@parameters$ePara,
                           ePara_name = a@parameters$ePara_name,
                           Method = Method,
                           MethodName = MethodName,
                           strats = a@parameters$Strategy,
                           dval = a@parameters$dval,
                           append_ = 0)
  }
  if(!(hasArg(ePara)))
  {
    ePara <- a@parameters$ePara
  }
  else
  {
    c1 <-  prediction_errors(data = a@parameters$data,
                             nval = a@parameters$nval,
                             ePara = ePara,
                             ePara_name = ePara_name,
                             Method = Method,
                             MethodName = MethodName,
                             strats = a@parameters$Strategy,
                             dval = a@parameters$dval,
                             append_ = 0)
  }

  if(!(hasArg(ePara_name)))
  {
    ePara_name <- a@parameters$ePara_name
  }


  if(!is.null(b))
  {
    if(!is.null(a@output$Error_Parameters))
    {
      a@output$Error_Parameters <- rbind(a@output$Error_Parameters,b@output$Error_Parameters)

    }
    if(!is.null(a@output$Predicted_Values))
    {
      a@output$Predicted_Values <-  rbind(a@output$Predicted_Values, b@output$Predicted_Values)
    }

  }
  a@parameters$ePara<-c(a@parameters$ePara,ePara)
  a@parameters$Method <- Method
  a@parameters$MethodName <-  MethodName
  a@parameters$ePara_name<-c(a@parameters$ePara_name,ePara_name)
  z=1

  if(!is.null(c1))
  {
    if(!is.null(a@output$Error_Parameters))
    {
      a@output$Error_Parameters<-cbind(a@output$Error_Parameters,c1@output$Error_Parameters)
    }

  }
  a@parameters$Method <- c(M1,a@parameters$Method)
  a@parameters$MethodName <- c(M2,a@parameters$MethodName)


  a@parameters$Method <- a@parameters$Method[!duplicated(a@parameters$Method)]
  a@parameters$MethodName <- a@parameters$MethodName[!duplicated(a@parameters$MethodName)]
  a@parameters$ePara <- a@parameters$ePara[!duplicated(a@parameters$ePara)]
  a@parameters$ePara_name<-a@parameters$ePara_name[!duplicated(a@parameters$ePara_name)]

  a@output$Predicted_Values <- unique(a@output$Predicted_Values)
  return(a)
}



