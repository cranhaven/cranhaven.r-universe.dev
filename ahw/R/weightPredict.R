#' @title Continuous time weight estimation based on
#' \code{\link[timereg]{predict.aalen}}
#' @description Extracts cumulative hazard estimates for each individual. Each
#' individual receives a weight process evaluated at pre-specified time points.
#' The weight process is estimated as a cumulative product involving estimated
#' cumulative hazard increments, and a hazard ratio estimated using a smoothing
#' parameter \code{b}.
#' @param fPred \code{\link[timereg]{predict.aalen}} object of the factual fit
#' @param cfPred \code{\link[timereg]{predict.aalen}} object of the
#' counterfactual fit
#' @param wtFrame \code{\link{data.frame}} or \code{\link{data.table}} for the
#' at risk individuals
#' @param ids All individuals in the data set
#' @param eventTimes Observed event times
#' @param eventIds Individuals that experience the event
#' @param b Smoothing parameter
#' @return data.table
#' @author Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
# Creating data.table with the individual predicted cumulative hazard increments.
# Estimates weights at the provided event times.
weightPredict <- function(fPred,cfPred,wtFrame,ids,eventTimes,eventIds,b){

        # willPlotWeights <- match.arg(willPlotWeights,choices=willPlotWeights,several.ok = T)

        fPredTimes <- fPred$time;cfPredTimes <- cfPred$time

        totTimes <- sort(unique(c(fPredTimes,cfPredTimes,eventTimes)))
        nTimes <- length(totTimes)
        lagtimes <- totTimes-b;lagtimes[lagtimes<0] <- 0
        lagtimes2 <- totTimes-2*b;lagtimes2[lagtimes2<0] <- 0
        lagtimes3 <- totTimes-3*b;lagtimes3[lagtimes3<0] <- 0
        lagInds <- sapply(lagtimes,function(tm)max(which(tm >= totTimes)))
        lagInds2 <- sapply(lagtimes2,function(tm)max(which(tm >= totTimes)))
        lagInds3 <- sapply(lagtimes3,function(tm)max(which(tm >= totTimes)))

        sortedEventTimes <- sort(eventTimes)

        mtf <- match(fPredTimes,totTimes)
        mtcf <- match(cfPredTimes,totTimes)
        mtf <- rep(mtf)

        dA_f <- as.vector(apply(fPred$S0,1,function(rw)-diff(c(0,log(rw)))))
        dA_cf <- as.vector(apply(cfPred$S0,1,function(rw)-diff(c(0,log(rw)))))


        predTable <- data.table(rowNum=rep(1:nrow(wtFrame),each=nTimes),
                                id=rep(wtFrame$id,each=nTimes),
                                to=rep(totTimes,nrow(wtFrame)),dA_f=0,dA_cf=0,
                                fEvent=rep(1*(1:nTimes %in% mtf),nrow(wtFrame)),
                                cfEvent=rep(1*(1:nTimes %in% mtcf),nrow(wtFrame)),
                                lagInd=rep(lagInds,nrow(wtFrame)),
                                lagInd2=rep(lagInds2,nrow(wtFrame)),
                                lagInd3=rep(lagInds3,nrow(wtFrame)))

        predTable[predTable$fEvent==1]$dA_f <- dA_f
        predTable[predTable$cfEvent==1]$dA_cf <- dA_cf

        # If there are several at risk states for each individual
        numRepId <- as.numeric(table(wtFrame$id))

        wtFrame$rowNum <- 1:nrow(wtFrame)

        # to <- predTable[id==idds]$to
        with(predTable, {
                predTable[,"numIdRow":= rep(cumsum(numRepId),times=nTimes*numRepId)]
                predTable[,"keep":=1*(to>= wtFrame$from[rowNum[1]==wtFrame$rowNum] &
                                        to< wtFrame$to[rowNum[1]==wtFrame$rowNum]),by=rowNum]
                predTable[keep==0 & predTable$numIdRow==rowNum,"keep":=1*(to>= max(wtFrame$to[id[1]==wtFrame$id])),by=id]

                # NB! Not keep every row that starts at to=0!
                # predTable[to==0,keep:=1]
                predTable[to==0 & predTable$numIdRow != rowNum,"keep":=1]
        })

        predTable <- predTable[predTable$keep==1]

        with(predTable, {
                predTable[,"EventTimes":=0]
                predTable[,"event":=0]
                predTable[,"EventId":=1*(id%in%eventIds),by=id]

                predTable[predTable$EventId==1,"EventTimes":=eventTimes[eventIds==id],by=id]
                predTable[predTable$EventId==1,"event":= 1*(EventTimes == to)]
                predTable[,"takeOut":=1*(to%in%eventTimes)]
                predTable[to==0,"takeOut":=1]
        })

        # Evaluating predicted cumulative hazards at (lagged) event times
        predTable <- predTable[,c("A_f","A_cf") := .(cumsum(dA_f),cumsum(dA_cf)),by=predTable$id]
        with(predTable, {
                predTable[,c("A_f_lag","A_cf_lag") := .(A_f[lagInd],A_cf[lagInd]),by=id]
                predTable[,c("A_f_lag2","A_cf_lag2") := .(A_f[lagInd2],A_cf[lagInd2]),by=id]
                predTable[,c("A_f_lag3","A_cf_lag3") := .(A_f[lagInd3],A_cf[lagInd3]),by=id]

                # Calculating jump term - contribution from the treatment
                # Using difference method based on 0, 1*b, 2*b, and 3*b steps back in time:
                predTable[,"jumpTerm":= (11/2*A_cf - 3*A_cf_lag - 3/2*A_cf_lag2 - A_cf_lag3)/(11/2*A_f - 3*A_f_lag - 3/2*A_f_lag2 - A_f_lag3)]

                # Convex modification for small times
                predTable[to<3*b,"jumpTerm":=(3*b-to)/(3*b) + (to/(3*b))*jumpTerm]

                predTable[,"jumpTerm":=jumpTerm - 1]
                predTable[event==0,"jumpTerm":=0]

                # Checking for "invalid" terms (e.g. 0/0)
                #numNaIds <- nrow(predTable[jumpTerm %in% c(NA,NaN,Inf),])
                #if(numNaIds != 0)
                #        cat('Warning: b is small for', numNaIds, 'individuals. Consider increasing b. \n')

                # Weight calculation; solving the SDE
                predTable[,"preweight" := 1 + dA_f - dA_cf + jumpTerm]
                predTable[,"weights":=cumprod(preweight),by=id]
        })

        predTable <- predTable[predTable$takeOut==1,]
        predTable <- predTable[,names(predTable) %in% c("id","to","weights"),with=F]

        # Estimators evaluate weights in the left limit:
        names(predTable)[names(predTable)=="to"] <- "from"

        return(predTable)
}
