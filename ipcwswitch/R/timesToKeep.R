#' Function to keep all event times
#             and times of changes in measurements of time-dpt covariates
#'
#' @param data dataframe containing the following variables
#' @param id patient's id
#' @param tstart date of the beginning of the follow-up (in Date format)
#' @param tstop date of the end of the follow-up (in Date format)
#' @param mes.cov list of vectors, each of them must contain the names (in character format)
#'  of the repeated measurements related to one time-dependent covariate
#' @param time.cov list of vectors, each of them must contain the times (in Date format)
#'  of the date when the abovementioned measurements were done
#'
#' @return list of two lists, one in Date format the other in numeric format.
#' Each of them contains, for each patient, the event time and
#' the times of changes in time-varying covariates
#' @export
#'
#' @references Graffeo, N., Latouche, A., Le Tourneau C., Chevret, S. (2019) "ipcwswitch: an R package for inverse probability of censoring weighting with an application to switches in clinical trials". Computers in biology and medicine, 111, 103339. doi : "10.1016/j.compbiomed.2019.103339"
#'
#' @examples kept.t <- timesTokeep(toydata, id = "id",
#' tstart = "randt", tstop = "lastdt",
#' mes.cov = list(c("ps1", "ps2", "ps3")),
#' time.cov = list(c("randt", "dt2", "dt3")))
#' # For example, for patient id=3, to obtain the kept times in Date format:
#' kept.t[[1]][[3]]
#' # To obtain the kept times in numeric format:
#' kept.t[[2]][[3]]
#' @seealso \code{\link{SHIdat}}

timesTokeep <- function(data, id, tstart, tstop,
                        mes.cov, time.cov){

  # number of time-dpt confounders ####
  L.cov <- length(mes.cov)
  L.cov.bis <- length(time.cov)
  if(L.cov != L.cov.bis)
    stop("Same numbers of measures and times of measurement are required!")

  # Maximum follow-up ####
  Tend <- data[, tstop]

  # browser()

  # Retain date when changes occur for all time-dpt cov ####
  # Split by patient --> loop on id
  tabi <- split(data, data[,id])
  L.tabi <- length(tabi)

  times <- vector()

  Keep <- list()
  keep.times <- list()
  keep.times.num <- list()

  for (i in 1:L.tabi) {
    keep.times[[i]] <- tabi[[i]][, tstart]
    for(m in seq(L.cov)){
      #if(!all(is.na(tabi[[i]][, mes.cov[[m]]])) & (tabi[[i]][, time.cov[[m]][1]] <= Tend[i])){
      if(!all(is.na(tabi[[i]][, mes.cov[[m]]])) & !all(is.na(tabi[[i]][, time.cov[[m]]])) & (tabi[[i]][, time.cov[[m]]][!is.na(tabi[[i]][, time.cov[[m]]])][1] <= Tend[i])){
        mytimes <- vector()
        vect.dat <- vector()
        # only keep not missing dates happening before Tend
        for (dat in time.cov[[m]]) {
          d <- tabi[[i]][, dat]
          class.d <- class(d)
          if (!is.na(d) & (d <= Tend[i])) {
            mytimes <- c(mytimes, d)     # value of the visit date
            class(mytimes) <- class.d         # in format Date
            vect.dat <- c(vect.dat, dat) # name of the visit date
          }
        }

        # ordered dates -- corresponding time-dpt measures
        ord         <- order(mytimes)
        mytimes     <- mytimes[ord]
        vect.dat    <- vect.dat[ord]
        # corresponding cov
        vect.cov <- mes.cov[[m]][time.cov[[m]] %in% vect.dat]
        vect.cov <- vect.cov[ord]

        # keep 1st time of measurement if not measured at tstart
        # and if value at 1st measurement different from that imputed at tstart
        # Note: in our case, these values were set to 0
        if(!is.na(mytimes[1]) & (mytimes[1]!=tabi[[i]][, tstart]) &
           !(mytimes[1]%in%keep.times[[i]]) &
           (!is.na(tabi[[i]][, vect.cov[1]])) &
          (tabi[[i]][, vect.cov[1]] != 0) ){ # to change if the imputed vaue at tstart is not 0
          keep.times[[i]] <- c(keep.times[[i]], mytimes[1])
        }
        # keep times when there is a change
        if(length(vect.cov) != 1){
          tempo1 <- tabi[[i]][, vect.cov[1]] # value of 1st measurement of the time-dpt cov
          temp.vect1 <- vect.cov[1]          # corresponding name
          tempo.time1 <- mytimes[1]          # correspond. date of measurement
          for (k in 2:length(vect.dat)) {
            # vect.cov : retain date when change occurs between (k-1) and k
            if (!is.na(tabi[[i]][, vect.cov[k]])) {
              tempo2 <- tabi[[i]][, vect.cov[k]]
              temp.vect2 <- vect.cov[k]
              tempo.time2 <- mytimes[k]
              if ((tempo.time1 != tempo.time2) &
                  (tempo1 != tempo2) &
                  !(tempo.time2 %in% keep.times[[i]])) {
                keep.times[[i]] <- c(keep.times[[i]], tempo.time2)
              }
              tempo1 <- tempo2
              temp.vect1 <- temp.vect2
              tempo.time1 <- tempo.time2
            }
          }
        }

      }
    }
    # add Tend
    class.keep <- class(keep.times[[i]])

    if(!(Tend[i] %in% keep.times[[i]])){
      keep.times[[i]] <- c(keep.times[[i]], Tend[i])
    }
    class(keep.times[[i]]) <- class.keep

  }

  Keep[[1]] <- keep.times

  for (i in 1:L.tabi) {
    ref.start <- tabi[[i]][, tstart]
    keep.times.num[[i]] <- keep.times[[i]]-ref.start
  }
  Keep[[2]] <- keep.times.num

  return(Keep)
}
