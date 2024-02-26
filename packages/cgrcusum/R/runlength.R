#' Determine run length of a CUSUM chart
#'
#' This function can be used to calculate the run length of a 'cgrcusum', 'bkcusum'
#' or 'bercusum'
#' chart when using control limit h
#'
#'
#' @param chart a 'cgrcusum', 'bkcusum' or 'bercusum' chart
#' @param h control limit h to be used when determining the run length
#' @param ... other parameters
#'
#' @return The run length of the chart with the given control limit.
#'
#'
#' @author Daniel Gomon
#' @examples
#' varsanalysis <- c("age", "sex", "BMI")
#' exprfitber <- as.formula(paste("(entrytime <= 365) & (censorid == 1)~",
#'                                paste(varsanalysis, collapse='+')))
#' surgerydat$instance <- surgerydat$Hosp_num
#' glmmodber <- glm(exprfitber, data = surgerydat, family = binomial(link = "logit"))
#' bercus <- bercusum(data = subset(surgerydat, Hosp_num == 14), glmmod = glmmodber,
#'                    followup = 100, theta = log(2))
#' runlength(bercus, h = 2)


#' @export
runlength <- function(chart, h){
  UseMethod("runlength")
}


#' @describeIn runlength Determine runlength of "cgrcusum" object
#' @family utils
#' @export
runlength.cgrcusum <- function(chart, h, ...){
  if(missing(h)){
    stop("Please specify a control limit h.")
  }
  if(missing(chart)){
    stop("Please provide a 'cgrcusum', 'bkcusum' or 'bercusum' chart as input.")
  }
  ind <- which(chart$CGR[,2] >= h)[1]
  if(is.na(ind)){
    return(Inf)
  } else{
    return(chart$CGR[ind,1])
  }
}





#' @describeIn runlength Determine runlength of "bkcusum" object
#' @family utils
#' @export
runlength.bkcusum <- function(chart, h, ...){
  if(missing(h)){
    stop("Please specify a control limit h.")
  }
  if(missing(chart)){
    stop("Please provide a 'cgrcusum', 'bkcusum' or 'bercusum' chart as input.")
  }
  ind <- which(chart$BK[,2] >= h)[1]
  if(is.na(ind)){
    return(Inf)
  } else{
    return(chart$BK[ind,1])
  }
}

#' @describeIn runlength Determine runlength of "bercusum" object
#' @family utils
#' @export
runlength.bercusum <- function(chart, h, ...){
  if(missing(h)){
    stop("Please specify a control limit h.")
  }
  if(missing(chart)){
    stop("Please provide a 'cgrcusum', 'bkcusum' or 'bercusum' chart as input.")
  }
  ind <- which(chart$CUSUM[,2] >= h)[1]
  if(is.na(ind)){
    return(Inf)
  } else{
    return(chart$CUSUM[ind,1])
  }
}




