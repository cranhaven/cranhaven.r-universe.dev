#' Computing the stabilized IPCweights
#'
#' @param data a dataframe containing the following variables
#' @param id the patient's id
#' @param tstart the date of the beginning of the follow-up (in numeric format, with the first being equal at 0)
#' @param tstop the date of the end of the follow-up (in numeric format)
#' @param cens the indicator of treatment censoring (denoted by 1 at the end of the follow-up)
#' @param arm the randomized treatment (2-levels factor)
#' @param bas.cov a vector the baseline covariates
#' @param conf a vector of time-dependent confounders
#' @param trunc an optional fraction for the weights. For instance, when trunc = 0.01,
#' the left tail is truncated to the 1st percentile and the right tail is truncated to the 99th percentile
#' @param type a character string specifying the type of survival curve. The default is \code{type=`kaplan-meier`}
#'
#' @return the initial dataframe data with stabilized IPCweights as additional arguments. By default, the un-truncated stabilized weights are given. If the trunc option is not NULL then the truncated stabilized weights are also given.
#' @export
#'
#' @importFrom stats quantile
#' @importFrom survival survfit
#' @importFrom survival coxph
#'
#' @references Graffeo, N., Latouche, A., Le Tourneau C., Chevret, S. (2019) "ipcwswitch: an R package for inverse probability of censoring weighting with an application to switches in clinical trials". Computers in biology and medicine, 111, 103339. doi : "10.1016/j.compbiomed.2019.103339"
#'
#' @examples
#' ## Not run
#' # ipcw(toy.rep, tstart = tstart, tstop = tstop, cens = cens,
#' # arm="arm",
#' # bas.cov = c("age"),
#' # conf = c("TDconf"), trunc = 0.05)
#'
#' # see ?SHIdat for a complete example
#' @seealso \code{\link{SHIdat}}
ipcw <- function(data, id, tstart, tstop, cens, arm, bas.cov, conf, trunc=NULL, type = "kaplan-meier"){
    
    tempcall <- match.call()
    
    data$weights <- vector("numeric", length = nrow(data))
    
    levArms <- levels(data[, arm])
    
    # ~ baseline covariates in numerator
    num <- paste(c(bas.cov), collapse = "+")
    # ~ baseline covariates and time-dependent confoundersin the denominator
    denom <- paste(c(bas.cov, conf), collapse = "+")
    
    if (length(levArms) > 2) {
        stop("Method not implemented yet for more than 2 arms!")
    } else if (length(levArms) < 2) {
        # if numerator
        if (length(bas.cov) != 0) {
            # Cox model for stabilized weights
            fit.cox.num <- coxph(formula = eval(parse(text = paste("Surv(",
                                                                   deparse(tempcall$tstart), ", ",
                                                                   deparse(tempcall$tstop), ", ",
                                                                   deparse(tempcall$cens), ") ~  ",
                                                                   num, sep = "" ))),
                                  data = data)
            fit.cox.denom <- coxph(eval(parse(text = paste("Surv(",
                                                           deparse(tempcall$tstart), ", ",
                                                           deparse(tempcall$tstop), ", ",
                                                           deparse(tempcall$cens), ") ~  ",
                                                           denom, sep = ""))),
                                   data = data)
            idpat <- unique(data[, id])
            for (i in idpat) {
                datai <- data[data[, id] == i, ]
                km.num <- survfit(fit.cox.num, newdata = datai, individual = TRUE, type = type)
                km.denom <- survfit(fit.cox.denom, newdata = datai, individual = TRUE, type = type)
                data$weights[data[, id] == i] <-
                    summary(km.num, times = datai[, as.character(tempcall$tstart)])$surv / summary(km.denom,
                                                                                                   times = datai[, as.character(tempcall$tstart)])$surv
            }
        } else{
            # if denominator only
            fit.cox.denom <- coxph(eval(parse(text = paste("Surv(",
                                                           deparse(tempcall$tstart), ", ",
                                                           deparse(tempcall$tstop), ", ",
                                                           deparse(tempcall$cens), ") ~  ",
                                                           denom, sep = ""))),
                                   data = data)
            idpat <- unique(data[, id])
            for (i in idpat) {
                datai <- data[data[, id] == i, ]
                km.denom <- survfit(fit.cox.denom, newdata = datai, individual = TRUE, type = type)
                data$weights[data[, id] == i] <-
                    1 / summary(km.denom, times = datai[, as.character(tempcall$tstart)])$surv
            }
        }
    } else{
        # if numerator
        if (length(bas.cov) != 0) {
            # For the first arm
            data1 <- data[data[, arm] == levArms[1],]
            fit.cox.num.1 <- coxph(formula = eval(parse(text = paste("Surv(",
                                                                     deparse(tempcall$tstart), ", ",
                                                                     deparse(tempcall$tstop), ", ",
                                                                     deparse(tempcall$cens), ") ~  ",
                                                                     num, sep = ""))),
                                   data = data1)
            fit.cox.denom.1 <- coxph(eval(parse(text = paste("Surv(",
                                                             deparse(tempcall$tstart), ", ",
                                                             deparse(tempcall$tstop), ", ",
                                                             deparse(tempcall$cens), ") ~  ",
                                                             denom, sep = ""))),
                                     data = data1)
            
            idpat1 <- unique(data1[, id])
            for (i in idpat1) {
                datai <- data1[data1[, id] == i, ]
                
                km.num <- survfit(fit.cox.num.1, newdata = datai, individual = TRUE, type = type)
                km.denom <- survfit(fit.cox.denom.1, newdata = datai, individual = TRUE, type = type)
                data1$weights[data1[, id] == i] <-
                    summary(km.num, times = datai[, as.character(tempcall$tstart)])$surv / summary(km.denom,
                                                                                                   times = datai[, as.character(tempcall$tstart)])$surv
            }
            
            # For the second arm
            data2 <- data[data[, arm] == levArms[2],]
            fit.cox.num.2 <- coxph(eval(parse(text = paste("Surv(",
                                                           deparse(tempcall$tstart), ", ",
                                                           deparse(tempcall$tstop), ", ",
                                                           deparse(tempcall$cens), ") ~  ",
                                                           num, sep = ""))), 
                                data = data2)
            fit.cox.denom.2 <- coxph(eval(parse(text = paste("Surv(",
                                                             deparse(tempcall$tstart), ", ",
                                                             deparse(tempcall$tstop), ", ",
                                                             deparse(tempcall$cens), ") ~  ",
                                                             denom, sep = ""))),
                                     data = data2)
            
            idpat2 <- unique(data2[, id])
            for (i in idpat2) {
                datai <- data2[data2[, id] == i, ]
                
                km.num <- survfit(fit.cox.num.2, newdata = datai, individual = TRUE, type = type)
                km.denom <- survfit(fit.cox.denom.2, newdata = datai, individual = TRUE, type = type)
                data2$weights[data2[, id] == i] <-
                    summary(km.num, times = datai[, as.character(tempcall$tstart)])$surv / summary(km.denom,
                                                                                                   times = datai[, as.character(tempcall$tstart)])$surv
            }
            rows.1 <- which(data[, arm] == levArms[1])
            data$weights[rows.1] <- data1$weights
            rows.2 <- which(data[, arm] == levArms[2])
            data$weights[rows.2] <- data2$weights
        } else{
            # if denominator only
            # arm 1
            data1 <- data[data[, arm] == levArms[1],]
            fit.cox.denom.1 <- coxph(eval(parse(text = paste("Surv(",
                                                             deparse(tempcall$tstart), ", ",
                                                             deparse(tempcall$tstop), ", ",
                                                             deparse(tempcall$cens), ") ~  ",
                                                             denom, sep = ""))),
                                     data = data1)
            
            idpat1 <- unique(data1[, id])
            for (i in idpat1) {
                datai <- data1[data1[, id] == i, ]
                km.denom <- survfit(fit.cox.denom.1, newdata = datai, individual = TRUE, type = type)
                data1$weights[data1[, id] == i] <-
                    1 / summary(km.denom, times = datai[, as.character(tempcall$tstart)])$surv
            }
            
            # arm 2
            data2 <- data[data[, arm] == levArms[2],]
            fit.cox.denom.2 <- coxph(eval(parse(text = paste("Surv(",
                                                             deparse(tempcall$tstart), ", ",
                                                             deparse(tempcall$tstop), ", ",
                                                             deparse(tempcall$cens), ") ~  ",
                                                             denom, sep = ""))),
                                     data = data2)
            
            idpat2 <- unique(data2[, id])
            for (i in idpat2) {
                datai <- data2[data2[, id] == i, ]
                km.denom <- survfit(fit.cox.denom.2, newdata = datai, individual = TRUE, type = type)
                data2$weights[data2[, id] == i] <-
                    1 / summary(km.denom, times = datai[, as.character(tempcall$tstart)])$surv
            }
            # rows with 1st level of arm
            rows.1 <- which(data[, arm] == levArms[1])
            data$weights[rows.1] <- data1$weights
            # rows with 2nd level of arm
            rows.2 <- which(data[, arm] == levArms[2])
            data$weights[rows.2] <- data2$weights
        }
        
    }
    
    # Truncated weights (optional)
    if (!(is.null(tempcall$trunc))) {
        data$weights.trunc <- data$weights
        data$weights.trunc[data$weights <= quantile(data$weights,
                                                    0 + trunc)] <-
            quantile(data$weights, 0 + trunc)
        data$weights.trunc[data$weights > quantile(data$weights,
                                                   1 - trunc)] <-
            quantile(data$weights, 1 - trunc)
    }
    return(data)
}