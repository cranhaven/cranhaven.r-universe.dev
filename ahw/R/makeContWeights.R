#' @title Continuous time weight estimation based on \code{aalen.predict}
#' @description Refines data longitudinally in order to do estimate parameters(e.g. Nelson-Aalen or Kaplan-Meier) using continuous time weights. The weights can be assessed using the plot option.
#' @param faFit The \code{\link[timereg]{aalen}} fit for the factual hazard
#' @param cfaFit The  \code{\link[timereg]{aalen}} fit for the hypothetical hazard
#' @param dataFr \code{\link{data.frame}} or \code{\link{data.table}} on long format
#' @param atRiskState At risk state for the event of interest
#' @param eventState State for the event of interest
#' @param startTimeName Name of column with start time of the at risk period
#' @param stopTimeName Name of column with stop time of the at risk period
#' @param startStatusName Name of the column that contains the starting state for each interval
#' @param endStatusName Name of the column that contains the end state for each interval
#' @param idName Name of column in \code{dataFr} that identifies individuals
#' @param b Smoothing bandwidth parameter
#' @param weightRange Truncates weights outside this interval
#' @param willPlotWeights Plot indicator
#' @param withLeftLim Calculates left limit at jump if desired
#' @return Longitudinally refined \code{\link{data.table}} of the initial \code{dataFr} with \code{weights} column added.
#' @references \url{https://arxiv.org/abs/1802.01946}
#' @author Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
#' @example inst/examples/makeContWeights_example.R
#' @export
makeContWeights <- function(faFit,cfaFit,dataFr,atRiskState,eventState,startTimeName,stopTimeName,startStatusName,endStatusName,idName,b,weightRange = c(0,10),willPlotWeights=TRUE,withLeftLim=FALSE){

        if(!is(faFit, "aalen") | !is(cfaFit, "aalen")) {
          stop("The survival fits must be of type aalen.",call. = FALSE)
        }
        if(!(is(dataFr, "data.table"))) {
          dataFr <- as.data.table(dataFr)
        }

        # Making new names for convenience
        namesMatch <- match(c(startStatusName,endStatusName,startTimeName,stopTimeName,idName),names(dataFr))
        saveNames <- names(dataFr)[namesMatch]
        names(dataFr)[namesMatch] <- c("from.state","to.state","from","to","id")

        # data frame to get predictions along
        wtFrame <- dataFr[dataFr$from.state %in% atRiskState,]

        # Times we want to estimate the weights at
        eventTimes <- wtFrame$to[wtFrame$to.state %in% eventState]

        # Checking whether there are ties in the input data
        if(length(unique(eventTimes)) != length(eventTimes)){
          stop("Error! Remove ties from the data. Apply function addNoiseAtEventTimes() to dataFr and try again.
               Run example(makeContWeights) for an example.")
        }



        pft <- predict(faFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0)

        ids <- unique(dataFr$id)
        eventIds <- wtFrame$id[wtFrame$to.state %in% eventState]



        # Obtain estimated weights
        fPred<- pft;cfPred  <- cpft
        weightFrame <- weightPredict(pft,cpft,wtFrame,ids,eventTimes,eventIds,b)

        # Refining the data.frame for individuals at risk
        Table <- refineTable(dataFr,atRiskState,eventState)

        # Merge so that Table includes the weight estimates
        Table <- merge(Table,weightFrame,by=c("id","from"),all.x=TRUE)


        # Calculates left limit at jump if desired. Can be improved
        if (withLeftLim) {
                with(Table, {
                        Table[id %in% eventIds & to.state %in% eventState,"toI":=to]
                        Table[,"toI":=toI[toI!=0],by=id]
                })
                for (i in 1:length(eventIds)) {
                        Table[Table$id==i & Table$toI!=0,]$weights <- weightFrame[weightFrame$id %in% eventIds[i] & weightFrame$from<=Table[Table$toI!=0,][1,]$to][.N,]$weights
                }
        }

        # Individuals weight constant after time of treatment
        with(Table, Table[Table$isAtRiskForTreatment != 1,"weights" := weights[1],by=id])


        Table <- subset(Table,select= !(names(Table) %in% c("rowNumber","numRep","putEventTimes","isAtRiskForTreatment","eventTime")))

        with(Table, Table[,"weights":=naReplace(weights),by=id])

        # Truncate weights that are outside a given range
        Table$weights[Table$weights < weightRange[1]] <- weightRange[1]
        Table$weights[Table$weights > weightRange[2]] <- weightRange[2]

        # Optional plot of the weight trajectories
        if(willPlotWeights == TRUE)
                plotContWeights(Table)

        # Switching names back
        namesMatch <- match(c("from.state","to.state","from","to","id"),names(Table))
        names(Table)[namesMatch] <- saveNames

        return(Table)

}
