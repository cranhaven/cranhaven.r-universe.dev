#' @title Plots mean and individual weight trajectories
#' @description Plotting all the individual weight trajectories in fr,
#' along with the mean. Plots weights for assessment.
#' @param fr Data with weight column
#' @param stopTimeName Name of column with stop time of the at risk period
#' @param startStatusName Name of the variable that contains the name of start
#' state for each at-risk interval
#' @param endStatusName Name of the variable that contains the name of end state
#' for each at-risk interval
#' @param idName Name of column in \code{dataFr} that identifies individuals
#' @author Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
#' @export
plotContWeights <- function(fr,stopTimeName="to",startStatusName="from.state",endStatusName="to.state",idName="id"){

        namesMatch <- match(c(startStatusName,endStatusName,stopTimeName,idName),names(fr))
        names(fr)[namesMatch] <- c("from.state","to.state","to","id")
        fr <- as.data.table(fr)
        tms <- sort(unique(fr$to))
        tmx <- max(tms)


        idNs <- unique(fr$id)
        numIds <- length(idNs)
        fr[,weights:=c(weights[-1],weights[length(weights)]),by="id"]
        wtIn <- fr$weights[fr$id==idNs[1]]
        wt <- rep(NA,length(tms));wt[1]<-1
        wt[match(fr[fr$id==idNs[1]]$to,tms)] <- wtIn

        wt <- naReplace(wt)
        # wt <- na.locf0(wt)
        wpr <- wt

        ylm <- c(max(c(0, min(fr$weights))), min(c(5, max(fr$weights))))
        plot(tms,wt,type="l",xlim=c(0,tmx),ylim=ylm,col="grey",xlab="time",ylab="weights")
        for(i in 2:numIds){

                wtIn <- fr$weights[fr$id==idNs[i]]
                wt <- rep(NA,length(tms));wt[1]<-1
                wt[match(fr[fr$id==idNs[i]]$to,tms)] <- wtIn

                wt <- naReplace(wt)
                # wt <- na.locf0(wt)
                if(i <= 500)
                        lines(tms,wt,col="grey")
                if(any(is.na(wt)))
                        break
                wpr <- wpr + wt
        }
        wpr <- wpr/numIds
        lines(tms,wpr,col="red")
        cat('Mean weights: ',mean(wpr))
}
