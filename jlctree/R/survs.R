#' Defines the splitting function for a new splitting method of \code{rpart}.
#'
#' Defines the splitting function for a new splitting method of \code{rpart}.
#' Not to be called directly by the user. 
#'
#' @param y the response value as found in the formula that is passed in by \code{rpart}.
#'    Note that \code{rpart} will normally
#'    have removed any observations with a missing response.
#' @param wt the weight vector from the call, if any.
#' @param x vector of \code{x} values.
#' @param parms the vector or list (if any) supplied by the user as a
#'          \code{parms} argument to the call.
#' @param continuous if TRUE the \code{x} variable should be treated as continuous.
#'      The value of this parameter is determined by \code{rpart} automatically.
#'
#' @return See reference.
#'
#' @importFrom survival coxph Surv
#' @seealso \code{\link{surve},\link{survi}}
#' @references \url{https://cran.r-project.org/package=rpart/vignettes/usercode.pdf}
#' @export

survs <- function
(y, wt, x, parms, continuous)
{
    y <- data.frame(y)
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
    rootval <- get_node_val(formulay1, formulay2, y, 
                            lrt=parms$lrt, 
                            stable=parms$stable, cov.max=parms$cov.max)

    if (nevents <= parms$min.nevents*2 || rootval < parms$stop.thre){
        if (continuous){
            goodness <- rep(-Inf,nrow(y)-1); direction <- goodness;
        } else{
            ux <- sort(unique(x))
            goodness <- rep(-Inf,length(ux)-1); direction <- ux
        }

        return(list(goodness=goodness, direction=direction))
    }

    if (continuous) {
        # Continuous x variable
        n <- nrow(y)
        goodness <- rep(-Inf,n-1)
        direction <- double(n-1)
        for (i in 1:(n-1)) {
            if (x[i] != x[i+1]) {
                nel <- sum(y$event[1:i])
                ner <- sum(y$event[(i+1):n])
                if (nel <= parms$min.nevents || ner <= parms$min.nevents ){
                    result <- c(-Inf,0)
                } else{
                    result <- tryCatch({
                        leftval <- get_node_val(formulay1, formulay2, y[1:i,], 
                                                lrt= parms$lrt, 
                                                stable=parms$stable, cov.max=parms$cov.max)
                        rightval <- get_node_val(formulay1, formulay2, y[(i+1):n,], 
                                                 lrt= parms$lrt, 
                                                 stable=parms$stable, cov.max=parms$cov.max)
                        c(rootval - (leftval + rightval), sign(leftval-rightval))
                    }, error = function(e){ c(-Inf, sign(1))})
                }
                goodness[i] <- result[1]; direction[i] <- result[2]
            }
        }
        goodness <- goodness + parms$split.add
    } else {
        # Categorical X variable
        n <- nrow(y)
        ux <- sort(unique(x))
        nx <- length(ux)

        goodness <- rep(-Inf, nx-1)
        direction <- double(nx-1)

        # Need to sort the data
        xorder <- order(x); xtmp <- x[xorder]; ytmp <- y[xorder,]

        for (i in 1:(nx-1)){
            nextstart <- min(which(xtmp == ux[i+1]))
            nel <- sum(ytmp$event[1:(nextstart-1)])
            ner <- sum(ytmp$event[nextstart:n])
            if (nel <= parms$min.nevents | ner <= parms$min.nevents ){
                result <- c(-Inf,0)
            } else{
                result <- tryCatch({
                    leftval <- get_node_val(formulay1, formulay2, ytmp[1:(nextstart-1),], 
                                            lrt=parms$lrt, 
                                            stable=parms$stable, cov.max=parms$cov.max)
                    rightval <- get_node_val(formulay1, formulay2, ytmp[nextstart:n,], 
                                             lrt=parms$lrt, 
                                             stable=parms$stable, cov.max=parms$cov.max)
                    c(rootval - (leftval + rightval), sign(leftval-rightval))
                }, error = function(e){ c(-Inf, sign(1))})
            }
            goodness[i] <- result[1]
        }
        names(goodness) <- ux[1:(nx-1)]
        goodness <- goodness + parms$split.add
        direction <- ux
    }

    list(goodness=goodness, direction=direction)
}
