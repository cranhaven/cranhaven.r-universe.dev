#' Censoring patient initiating the other arm treatment and building a treatment censoring indicator cens
#'
#' @param data a dataframe containing the following variables
#' @param id the patient's id
#' @param tstart the date of the beginning of the follow-up (in numeric format)
#' @param tstop the date of the end of the follow-up (in numeric format)
#' @param event the indicator of failure (a death is denoted by 1 at the end of the follow-up)
#' @param censTime the chosen time to censor the patients (in numeric format)
#' @param arm the randomized treatment (2-levels factor)
#' @param realtrt the randomized treatment (2-levels factor)
#' @param trt.start the time of initiation of the randomized treatment (NULL by default)
#' @param trt.stop the time of termination of the randomized treatment (NULL by default)
#'
#' @return a dataframe in the long format, with the data being censored
#'  according to the input date, censTime. a treatment censoring indicator, cens,
#'  is thus added to the previous dataset to indicate such a switch.
#'  Note that this function provides the option to include in the data
#'  the treatment really taken with the corresponding dates.
#'  Then, the treatment really taken is a 3-levels factor,
#'  i.e., the two from the randomized arms and a third indicating
#'  the no-treatment case (None).
#' @export
#'
#' @references Graffeo, N., Latouche, A., Le Tourneau C., Chevret, S. (2019) "ipcwswitch: an R package for inverse probability of censoring weighting with an application to switches in clinical trials". Computers in biology and medicine, 111, 103339. doi : "10.1016/j.compbiomed.2019.103339"
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
#' # Before censoring:
#' toy.long
#' # Ater censoring:
#' toy.long2
#' @seealso \code{\link{SHIdat}}, \code{\link{timesTokeep}}, \code{\link{wideToLongTDC}}

cens.ipw <- function(data, id, tstart, tstop, event, censTime, arm, realtrt = FALSE,
                     trt.start = NULL, trt.stop = NULL){
  # work by patient
  tabi <- split(data, data[,id])

  L.tabi   <- length(tabi)
  newtabi <- list() # to be used only if realtrt=TRUE
  newtabi2 <- list() # to be used only if realtrt=TRUE

  tablist <- lapply(1:L.tabi, function(i){
    # keep lines when tstart is before censTime (if not missing)
    if(!is.na(tabi[[i]][1, censTime])){
      tabi[[i]] <- tabi[[i]][tabi[[i]][, tstart] < tabi[[i]][, censTime], ]
    }
    ltabi <- nrow(tabi[[i]])
    # if only one line and censTime<tstop then event = 0
    if(ltabi == 1 & (!is.na(tabi[[i]][ltabi, censTime])) &
       (tabi[[i]][ltabi, "tstop"] >  tabi[[i]][ltabi, censTime]) ){
        tabi[[i]][ltabi, event] <- 0
    }
    # for those lines, reimplace tstop by censTime
    tabi[[i]][ltabi, "tstop"] <- ifelse(!is.na(tabi[[i]][ltabi, censTime]),
                                        tabi[[i]][ltabi, censTime],
                                        tabi[[i]][ltabi, tstop])
    # censoring indicator
    tabi[[i]]$cens <- rep(0, ltabi)
    tabi[[i]]$cens[ltabi] <- ifelse(!is.na(tabi[[i]][ltabi, censTime]), 1, 0)

    # if User wants to introduce dates for initiation and end of treatment
    # (treatment really taken instead of assigned randomized arm)
    if(realtrt){
      # treatment that was really taken
      tabi[[i]]$trt <- tabi[[i]][,arm]
      levels(tabi[[i]]$trt) <- c(levels(tabi[[i]]$trt), "None")
      # the treatment was iniated at trt.start
      # => split the corresponding fu interval
      # Taking into account cases where trt.start = tstart
      if(tabi[[i]][1, trt.start] %in% tabi[[i]][, tstart]){
        newtabi[[i]] <- tabi[[i]]
        for(facname in names(tabi[[i]])[sapply(tabi[[i]], is.factor)]){
            newtabi[[i]][, facname] <- as.factor(tabi[[i]][,facname])
        }
        ind.min <- which(tabi[[i]][, tstart]==tabi[[i]][1, trt.start])
        if(ind.min != 1){
          newtabi[[i]][1:(ind.min-1), "trt"] <- "None"
        }
      }else{
        newtabi[[i]] <- data.frame(matrix(ncol = ncol(tabi[[i]]), nrow = ltabi + 1))
        colnames(newtabi[[i]]) <- colnames(tabi[[i]])
        for(facname in names(tabi[[i]])[sapply(tabi[[i]], is.factor)]){
           newtabi[[i]][, facname] <- factor(NULL,levels = levels(tabi[[i]][,facname]))
        }
        ind.insert <- max(which(tabi[[i]][, trt.start] > tabi[[i]][, tstart]))
        if(ind.insert == 1){
          # introduce a new line
          newtabi[[i]][1:2,] <- tabi[[i]][1, ]
          if(ltabi !=1){
            newtabi[[i]][3:(ltabi+1),] <- tabi[[i]][2:ltabi, ]
          }
          # if last row => event/cens should not be duplicated
          if(ind.insert==ltabi){
              newtabi[[i]][ind.insert, "event"] <- 0
              newtabi[[i]][ind.insert, "cens"] <- 0
          }
          # tstart and tstop depending on trt.start
          newtabi[[i]][1, "tstop"] <- tabi[[i]][1, trt.start]
          newtabi[[i]][2, "tstart"]<- tabi[[i]][1, trt.start]
          # trt really taken from 2nd row
          newtabi[[i]][1, "trt"] <- "None"
        }else{
          # keep lines before ind.insert
          newtabi[[i]][1:(ind.insert-1),] <- tabi[[i]][1:(ind.insert-1), ]
          # split in 2 rows
          newtabi[[i]][ind.insert:(ind.insert+1),] <- tabi[[i]][ind.insert,]
          # if last row => event/cens should not be duplicated
          if(ind.insert==ltabi){
              newtabi[[i]][ind.insert, "event"] <- 0
              newtabi[[i]][ind.insert, "cens"] <- 0
          }
          # remaining rows (if there are)
          if(ind.insert<ltabi){
              newtabi[[i]][(ind.insert+2):(ltabi+1),] <- tabi[[i]][(ind.insert+1):ltabi,]
          }
          # trt really taken from ind.insert / corresponding dates
          newtabi[[i]][ind.insert, "tstop"] <- tabi[[i]][1, trt.start]
          newtabi[[i]][(ind.insert+1), "tstart"] <- tabi[[i]][1, trt.start]
          newtabi[[i]][1:ind.insert, "trt"] <- "None"
        }
      }
      tabi[[i]] <- newtabi[[i]]
      # the treatment ended at trt.stop
      # => split the corresponding fu interval
      # Taking into account cases where trt.stop = tstart
          #browser()
      if(!is.na(tabi[[i]][1, trt.stop]) & (tabi[[i]][1, trt.stop] != tabi[[i]][nrow(tabi[[i]]), tstop])){
          ltabi2 <- nrow(newtabi[[i]])
          if(newtabi[[i]][1, trt.stop] %in% newtabi[[i]][, tstart]){
          newtabi2[[i]] <- newtabi[[i]]
          for(facname in names(newtabi[[i]])[sapply(newtabi[[i]], is.factor)]){
            newtabi2[[i]][, facname] <- as.factor(newtabi[[i]][,facname])
          }
          ind.min2 <- which(newtabi[[i]][, tstart]==newtabi[[i]][1, trt.stop])
          newtabi2[[i]][ind.min2:ltabi2, "trt"] <- "None"
        }else{
          newtabi2[[i]] <- data.frame(matrix(ncol = ncol(tabi[[i]]), nrow = ltabi2 + 1))
          colnames(newtabi2[[i]]) <- colnames(newtabi[[i]])
          for(facname in names(newtabi[[i]])[sapply(newtabi[[i]], is.factor)]){
            newtabi2[[i]][, facname] <- factor(NULL,levels = levels(newtabi[[i]][,facname]))
          }
          ind.insert <- max(which(newtabi[[i]][, trt.stop] > newtabi[[i]][, tstart]))
          if(ind.insert == ltabi2){
            # keep lines before ind.insert
            newtabi2[[i]][1:(ind.insert-1),] <- newtabi[[i]][1:(ind.insert-1), ]
            # split in 2 rows
            newtabi2[[i]][ind.insert:(ltabi2+1),] <- newtabi[[i]][ltabi2,]
            # last row => event/cens should not be duplicated
            newtabi2[[i]][ind.insert, "event"] <- 0
            newtabi2[[i]][ind.insert, "cens"] <- 0
            # trt really taken from ind.insert / corresponding dates
            newtabi2[[i]][ind.insert, "tstop"] <- newtabi[[i]][1, trt.stop]
            newtabi2[[i]][(ind.insert+1), "tstart"] <- newtabi[[i]][1, trt.stop]
            newtabi2[[i]][(ind.insert+1), "trt"] <- "None"
          }else{
            # keep lines before ind.insert
            newtabi2[[i]][1:(ind.insert-1),] <- newtabi[[i]][1:(ind.insert-1), ]
            # split in 2 rows
            newtabi2[[i]][ind.insert:(ind.insert+1),] <- newtabi[[i]][ind.insert,]
            # keep lines after ### reprendre ici !!! id=71
            newtabi2[[i]][(ind.insert+2):(ltabi2+1),] <- newtabi[[i]][(ind.insert+1):ltabi2,]
            # trt really taken from ind.insert / corresponding dates
            newtabi2[[i]][ind.insert, "tstop"] <- newtabi[[i]][1, trt.stop]
            newtabi2[[i]][(ind.insert+1), "tstart"] <- newtabi[[i]][1, trt.stop]
            newtabi2[[i]][(ind.insert+1):(ltabi2+1), "trt"] <- "None"
          }
        }
        tabi[[i]] <- newtabi2[[i]]
      }
    }
    return(tabi[[i]])
  })

  data2 <- do.call( rbind, tablist )
  return(data2)
}



