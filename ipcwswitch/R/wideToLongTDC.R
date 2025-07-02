#' Function from wide to long format
#'
#' @param data a dataframe containing the variables id, tstart, tstop, mes.cov and time.cov
#' @param id the patient's id
#' @param tstart date of the beginning of the follow-up (in Date format)
#' @param tstop date of the end of the follow-up (in Date format)
#' @param event the indicator of failure (a death is denoted by 1 at the end of the follow-up)
#' @param bas.cov a vector containing the names (in character format) of the baseline covariates
#' @param mes.cov a list of vectors, each of them must contain the names (in character format)
#' of the repeated measurements related to one time-dependent covariate
#' @param time.cov a list of vectors, each of them must contain the times (in Date format)
#' of the date when the abovementioned measurements were done
#' @param times a list of vectors. Each of them must contain, for each patient,
#' the event time and the times of changes in time-varying covariates
#'
#' @return the long format version of the initial dataframe data. The repeated values
#' included in each vector of the list mes.cov are aggregated in a variable named
#' aas the name of the corresponding list member.
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
#' toy.long
#' @seealso \code{\link{SHIdat}}, \code{\link{timesTokeep}}

wideToLongTDC <- function(data, id, tstart, tstop, event,
                          bas.cov, mes.cov, time.cov, times){

  # number of time-dpt confounders
  L.cov <- length(mes.cov)
  # number of times by patient
  ntimesP <- lapply(times, length)
  # data.frame initialisation ####
  nPat <- unique(data[, id])
  L.nPat <- length(nPat)
  Tend <- data[,tstop]
  datalong <- data.frame(id = rep(nPat, unlist(ntimesP)-1))


  # Split by patient ####
  tabi     <- split(datalong, datalong$id)
  L.tabi <- length(tabi)
  datatabi <- split(data, data[,id])
  tablist <- lapply(1:L.tabi, function(i){
    # number of rows in each id subset of datalong: ntimesP[[i]]
    ord <- order(times[[i]])
    times[[i]] <- times[[i]][ord]
      #browser()
    # start-stop fu
    tabi[[i]]$tstart <- times[[i]][-ntimesP[[i]]]
    tabi[[i]]$tstop  <- times[[i]][-1]
    # death indicator
    tabi[[i]]$event <- rep(0, ntimesP[[i]]-1)
    if(datatabi[[i]]$status==1)
        tabi[[i]]$event[ntimesP[[i]]-1] <- 1
    # time-fixed covariate var.baseline
    for(k in 1:length(bas.cov)){
      myvar <- bas.cov[k]
      tabi[[i]][ , myvar] <- vector("numeric", ntimesP[[i]]-1)
      tabi[[i]][ , myvar] <- rep(datatabi[[i]][1, myvar], ntimesP[[i]]-1)
    }
    # Time-dpt confounders
    for(m in seq(L.cov)){
      #browser()
      # creating a column with the mth name in the list of time-dpt conf
      tabi[[i]][,names(mes.cov)[m]] <- vector("numeric", ntimesP[[i]]-1)
      # order corresponding dates
      columns <- datatabi[[i]][, time.cov[[m]]]
      ord1 <- do.call('order', as.data.frame(t(columns)))
      dates <- time.cov[[m]][ord1]
      # corresponding ordered values of the time-dpt cov
      td.cf <- mes.cov[[m]][ord1]

      # print(c("m = ", m))

      for (q in 1:length(dates)) {
        dat <- datatabi[[i]][, dates[q]]
        if (!is.na(dat) &
            !is.na(datatabi[[i]][td.cf[q]])) {
          tabi[[i]][, names(mes.cov)[m]] <-
            unlist(ifelse(tabi[[i]]$tstart >= dat, datatabi[[i]][td.cf[q]],
                          tabi[[i]][, names(mes.cov)[m]]))
        }
      }

    }
    # print(c("i = ", i))
    # output
    return(tabi[[i]])
  })
  datalong <- do.call( rbind, tablist )
  return(datalong)

}
