#' Defines the evaluation function for a new splitting method of \code{rpart}.
#'
#' Defines the evaluation function for a new splitting method of \code{rpart}.
#'  Not to be called directly by the user. 
#'
#' @param y the response value as found in the formula that is passed in by \code{rpart}.
#'    Note that \code{rpart} will normally have removed any observations 
#'  with a missing response.
#' @param parms the vector or list (if any) supplied by the user as a
#'          \code{parms} argument to the call.
#' @param wt the weight vector from the call, if any.
#'
#' @return See reference.
#'
#' @seealso \code{\link{survs},\link{survi}}
#' @references \url{https://cran.r-project.org/package=rpart/vignettes/usercode.pdf}
#' @export

surve <- function
(y, wt, parms){

    y <-  data.frame(y)
    if (parms$LTRC){
        colnames(y)[1:4] <- c('start','end','event','biomarker')
        formulay1 <- Surv(start, end, event) ~ . - biomarker
        formulay2 <- Surv(start, end, event) ~ .
    } else {
        colnames(y)[1:3] <- c('end','event','biomarker')
        formulay1 <- Surv(end, event) ~ . - biomarker
        formulay2 <- Surv(end, event) ~ .
    }


    nevents <- sum(y[,'event'])
    if(nevents  <= parms$min.nevents){
        nodeval <- 0
    } else {
        nodeval <- get_node_val(formulay1, formulay2, y, 
                                lrt=parms$lrt, stable=parms$stable, cov.max=parms$cov.max)
    }
    # deviance: it should be closely related to the split criteria.
    # label: does not matter, but we use node_val here.
    list(label = nodeval, deviance = nodeval)
}


