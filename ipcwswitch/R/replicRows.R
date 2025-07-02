#' Function to replicate the rows so that each patients' follow-up is split
#' according to all event times (times parameter)
#' up to each patient's end time
#'
#' @param data a dataframe containing the following variables
#' @param tstart the date of the beginning of the follow-up (in numeric format, with the first being equal at 0)
#' @param tstop the date of the end of the follow-up (in numeric format)
#' @param event the indicator of failure (a death is denoted by 1 at the end of the follow-up)
#' @param cens the indicator of treatment censoring (denoted by 1 at the end of the follow-up)
#' @param times1 a vector of times (in numeric format) indicating the times according to which
#' the rows have to be split for patients in the first arm 
#' @param times2 a vector of times (in numeric format) indicating the times according to which
#' the rows have to be split for patients in the second arm 
#' @param arm the randomized treatment (2-levels factor)
#'
#' @return a formatted dataframe with the rows replicated according to the provided times parameter
#' @export
#'
#' @importFrom survival survSplit
#'
#' @references Graffeo, N., Latouche, A., Le Tourneau C., Chevret, S. (2019) "ipcwswitch: an R package for inverse probability of censoring weighting with an application to switches in clinical trials". Computers in biology and medicine, 111, 103339. doi : "10.1016/j.compbiomed.2019.103339"
#'
#'
#' @examples
#' # To obtain the times parameter, we can apply the timesTokeep function on the same
#' # dataframe in the wide format
#' kept.t <- timesTokeep(toydata, id = "id",
#' tstart = "randt", tstop = "lastdt",
#' mes.cov = list(c("ps1", "ps2", "ps3")),
#' time.cov = list(c("randt", "dt2", "dt3")))
#' # Now, we can build the long format
#' toy.long <- wideToLongTDC(data = toydata, id = "id",
#' tstart = "randt", tstop = "lastdt", event = "status",
#' bas.cov = c("age", "arm", "swtrtdt"),
#' mes.cov = list(TDconf = c("ps1", "ps2", "ps3")),
#' time.cov = list(c("randt", "dt2", "dt3")),
#' times = kept.t[[1]])
#' # Put dates in numeric format with tstart at 0
#' toy.long$tstart <- as.numeric(toy.long$tstart)
#' toy.long$tstop <- as.numeric(toy.long$tstop)
#' toy.long$swtrtdt <- as.numeric(toy.long$swtrtdt)
#' tabi <- split(toy.long, toy.long$id)
#' L.tabi   <- length(tabi)
#' tablist <- lapply(1:L.tabi, function(i){
#'     refstart <- tabi[[i]]$tstart[1]
#'     tabi[[i]]$tstart  <- tabi[[i]]$tstart - refstart
#'     tabi[[i]]$tstop <- tabi[[i]]$tstop - refstart
#'     tabi[[i]]$swtrtdt <- tabi[[i]]$swtrtdt - refstart
#'     return(tabi[[i]])
#'     })
#'     toy.long <- do.call( rbind, tablist )
#' # Patients are censored when initiating the other arm treatment, that is, at time swtrtdt
#' toy.long2 <- cens.ipw(toy.long, id = "id", tstart = "tstart", tstop = "tstop",
#' event = "event", arm = "arm",
#' realtrt = FALSE, censTime ="swtrtdt")
#' # We collect all event times (death for both arms and treatment censoring according to the trt arm)
#' rep.times1 <- unique(c(toy.long2$tstop[toy.long2$cens==1 & toy.long2$arm == "A"],
#' toy.long2$tstop[toy.long2$event==1]))
#' rep.times2 <- unique(c(toy.long2$tstop[toy.long2$cens==1 & toy.long2$arm == "B"],
#' toy.long2$tstop[toy.long2$event==1]))
#' # to put times in same order as arms levels
#' levels(toy.long2[, "arm"])
#' # Now, we can replicate the rows
#' toy.rep   <- replicRows(toy.long2, tstart = "tstart", tstop = "tstop",
#'                         event = "event", cens = "cens", 
#'                         times1 = rep.times1, times2 = rep.times2,
#'                         arm = "arm")
#' toy.rep
#' 
#' @seealso \code{\link{cens.ipw}}, \code{\link{SHIdat}}, \code{\link{timesTokeep}}, \code{\link{wideToLongTDC}}
replicRows <- function(data, tstart, tstop, event, cens, times1, times2, arm){
    
    tempcall <- match.call()
    data$weights <- vector("numeric", length = nrow(data))
    levArms <- levels(data[, arm])
    
    # arm 1
    data1 <- data[data[, arm] == levArms[1], ]
    # gathering all unique times
    if(length(times1) != 0){
        alltimes1 <- sort(unique(unlist(times1)))
        
        
        tabi <- split(data1, data1[,"id"])
        L.tabi   <- length(tabi)
        tablist1 <- lapply(1:L.tabi, function(i){
            l.tabi <- nrow(tabi[[i]])
            d1 <- list(); d2 <- list()
            for(j in seq(l.tabi)){
                cut.t <- alltimes1[(alltimes1 > tabi[[i]][j,tstart]) & (alltimes1 < tabi[[i]][j,tstop])]
                d1[[j]] <- survSplit(tabi[[i]][j,],
                                     cut = cut.t,
                                     end = tstop,
                                     event = event)
                d2[[j]] <- survSplit(tabi[[i]][j,],
                                     cut = cut.t,
                                     end = tstop,
                                     event = cens)
                d1[[j]]$cens <- d2[[j]]$cens
            }
            
            return(do.call(rbind, d1))
        })
        res1 <- do.call(rbind, tablist1) 
    }else{
        if(!is.na(levArms[1])){
            res1 <- data1
        }else{
            res1 <- NULL
        }
    } 
    
    
    # arm 2
    data2 <- data[data[, arm] == levArms[2], ]
    # gathering all unique times 
    if(length(times2) != 0){
        alltimes2 <- sort(unique(unlist(times2)))
        
        
        tabi <- split(data2, data2[,"id"])
        L.tabi   <- length(tabi)
        tablist2 <- lapply(1:L.tabi, function(i){
            l.tabi <- nrow(tabi[[i]])
            d1 <- list(); d2 <- list()
            for(j in seq(l.tabi)){
                cut.t <- alltimes2[(alltimes2 > tabi[[i]][j,tstart]) & (alltimes2 < tabi[[i]][j,tstop])]
                d1[[j]] <- survSplit(tabi[[i]][j,],
                                     cut = cut.t,
                                     end = tstop,
                                     event = event)
                d2[[j]] <- survSplit(tabi[[i]][j,],
                                     cut = cut.t,
                                     end = tstop,
                                     event = cens)
                d1[[j]]$cens <- d2[[j]]$cens
            }
            
            return(do.call(rbind, d1))
        })
        res2 <- do.call(rbind, tablist2)
    }else{
        if(!is.na(levArms[2])){
            res2 <- data2
        }else{
            res2 <- NULL
        }
    } 
    
    
    return(rbind(res1,res2))
    
}