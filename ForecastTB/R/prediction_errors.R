#' Function working as testbench for comparison of Prediction methods
#'
#' @param  data as input time series for testing
#' @param  nval as an integer to decide number of values to predict
#' @param  ePara as type of error calculation (RMSE and MAE are default), add an error parameter of
#'         your choice in the following manner: ePara = c("errorparametername")
#'         where errorparametername is should be a source/function which returns desired error set
#' @param ePara_name as list of  names of error parameters passed in order
#' @param  Method as the list of locations of function for the proposed prediction method
#'         (should be recursive) (default:arima)
#' @param  MethodName as list of names for function for the proposed prediction method in order
#' @param  strats as list of forecasting strategies. Available : recursive and dirrec
#' @param  append_ suggests if the function is used to append to another instance
#' @param  dval as last d values of the data to be used for forecasting
#' @name prediction_errors
#' @import ggplot2
#' @import forecast
#' @importFrom imputeTestbench rmse mae mape
#' @importFrom stats ts
#' @importFrom methods hasArg
#' @importFrom methods setClass
#' @importFrom methods new
#' @importFrom methods representation
#' @importFrom stats predict
#' @importFrom utils tail
#' @return Returns error comparison for forecasting methods
#' @export
#' @examples
#' prediction_errors(data = nottem)

#==================================================================================
# prediction_errors starts here....
#==================================================================================

prediction_errors <- function(data,nval,ePara,ePara_name,Method,MethodName,strats,dval,append_)
{

  class_cache <- new.env(parent = emptyenv())
  #if(!(hasArg(data)))
  #{data <- nottem} #default data is nottem

  if(!(hasArg(nval)))
  {nval <- 12} #default nval is 12

  if(!(hasArg(dval)))
  {dval <- length(data)} #default dval is entire data


  if(!(hasArg(strats)))
  {strats <- c("Recursive")} #default :recursive and dirrec

  train <- data[1:(length(data)-nval)]
  test <- data[(length(data)-nval+1):length(data)]

  # Set default values
  if(!(hasArg(ePara))){
    ePara <- c("RMSE","MAE", "MAPE")
    ePara_name <- c("RMSE","MAE", "MAPE")
  }  else if(append_ == "1"){   #if(!(hasArg(append_)))
      append_ = "1"
      ePara <- c("RMSE","MAE","MAPE",ePara)
      ePara_name <- c("RMSE","MAE","MAPE",ePara_name)
  }

  if(!(hasArg(Method)))
  {
    Method <- c("ARIMA")
    MethodName <- c("ARIMA")
  } else if(append_ == "1"){
      Method <- c("ARIMA",Method)
      MethodName <- c("ARIMA",MethodName)
    }

  #define empty list for list of dataframes for different strategies
  mylist=list()
  mylist1=list()

  l=1

  #if((strats[l])=="Recursive")
  while (!is.na(strats[l])){
    lst =c()
    #define data frame with row names as ePara_name
    eParam = ePara_name
    lst=data.frame(eParam,stringsAsFactors = FALSE)
    ca=c(1:(nval))
    cst = data.frame(ca)
    cst = t(cst)
    cst = rbind(cst,test);
    extime = vector(mode = "numeric", length=0)#define vector for execution time

    i=1
    k=1
    #cst = rbind(cst,test);
    while (!(is.na(Method[i])))
    { #append dataframe with columns of error values of given methods in order
      #fit model in order
      if(toupper(Method[i])=="ARIMA")
      {
        dT1 <- Sys.time()
        d <- strategy2(train, n, nval, dval, strats[l], Method[i])
        #d <- as.numeric(unlist(data.frame(d)[1]))
        dT2 <- Sys.time()
      } else{
        #parse method path
        dT1 <- Sys.time()
        d <- strategy2(train, n, nval, dval, strats[l], Method[i])
        dT2 <- Sys.time()
      }
      #cst[i,1]=d
      dT <- dT2 - dT1
      dT <- as.numeric(dT)

      extime = append(extime,dT)
      cst = rbind(cst,d)

      rownames(cst)[2]="Test values"
      rownames(cst)[i+2]=MethodName[i]

      k <- 1
      x = c()#define list of errors to be appended as column
      obs <- test
      pred <- d
      while (!is.na(ePara[k]))
      {
        if(toupper(ePara[k]) == "RMSE")
        {
          ghnew <- rmse(obs, pred)    #rmse(test, d)
        }
        else if(toupper(ePara[k])=="MAE")
        {
          ghnew <- mae(obs, pred)
        }
        else if(toupper(ePara[k])=="MAPE")
        {
          ghnew <- mape(test, d)
        }
        else
        {
          newPar <- parse(text = ePara[k])
          newPar <- eval(newPar)
          #newPar <- newPar$value(test, d)
          ghnew <- newPar
        }
        k=k+1
        x=c(x,ghnew)
      }
      lst=cbind(lst, as.data.frame(x))#bind the column to the data frame
      colnames(lst)[i+1] <- MethodName[i]
      #colnames(lst) <- c("eParam", MethodName[i])
      i=i+1
    }
    lst = rbind(lst, extime)
    lst[length(ePara)+1,1]="exec_time"
    lst=t(lst)
    mylist=list(mylist,lst)
    l=l+1
    cst = cst[-1,]
    mylist1=list(mylist1,cst)
  }
  values =list(data,nval,ePara,ePara_name,Method,MethodName,strats,dval)
  names(values)=c("data","nval","ePara","ePara_name","Method","MethodName","Strategy","dval")

  d=list( mylist[[2]], mylist1[[2]])
  names(d)=c("Error_Parameters","Predicted_Values")

  d$Error_Parameters<-as.data.frame(d$Error_Parameters)
  colnames(d$Error_Parameters)<-as.character(unlist(d$Error_Parameters[1,]))
  d$Error_Parameters=d$Error_Parameters[-1,]


  colnames(d$Predicted_Values)<-c(1:nval)
  #colnames(d$DIRREC_Predicted_Values)<-c(1:nval)

  #prediction_errors <- setClass(Class="prediction_errors", representation(output="list", parameters="list"))

  prediction_errors <- setClass("prediction_errors", slots = representation(output="list", parameters="list"), where = class_cache)

  #return(track("prediction_errors", output = d, parameters = values))
  return(new("prediction_errors", output = d, parameters = values))

}
