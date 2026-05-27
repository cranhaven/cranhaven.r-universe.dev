# #######################################
# ##   DATE MANAGER FUNCTION LIBRARY   ##
# #######################################
#
# #CONTAINS
#   # dateExtender()
#   # extendDates()
#   # makeDates() - produces dates data.frame (year, month, day columns)
#   # mod.get.date.ind() - grab a variety of date indices from multiple modelTags
#   # mod.get.date.ind.extnd() - grab a variety of date indices from multiple modelTags with date extension
#   # get.date.ind() - master function to grab a variety of date indices
#   # get.month.ind()
#   # get.year.ind()
#   # get.seas.ind() - (note seasons currently correspond to southern hemisphere)
#   # julian_day()
#   # split_ts()  - divide year into even(ish) chunks for harmonic fit
#   # get.period.ind() - groups indices with the same period assignment
#
##############################################################################
dateExtender <- function(obs = NULL,
                         simLengthNyrs = NULL,
                         file = NULL,
                         modelTag = NULL) {
  # EXTEND DATES IF NEEDED
  if (!is.null(simLengthNyrs)) {
    if (!(any(modelTag %in% c("Simple-ann", "Simple-seas")))) {
      dateExtnd <- extendDates(
        simLengthNyrs = simLengthNyrs,
        times = obs$times, timeStep = obs$timeStep
      )
      progress("Extending dates", file)
    } else {
      dateExtnd <- obs$times # make the same as observed
      progress("Length of time series cannot be increased using simple scaling", file)
    }
  } else {
    dateExtnd <- obs$times # make the same as observed
  }
  return(dateExtnd)
}

extendDates <- function(simLengthNyrs = NULL,
                        times = NULL,
                        timeStep = NULL) {
  nT <- length(times)

  if (is.null(timeStep)) {
    browser()
  }

  year <- as.integer(format(times, "%Y"))
  firstYr <- year[1]
  lastYr <- firstYr + simLengthNyrs - 1

  firstTimeFirstYr <- as.POSIXct(paste0(firstYr, "/1/1 0:0:0"), tz = "UTC")
  firstTimeAfterLastYr <- as.POSIXct(paste0((lastYr + 1), "/1/1 0:0:0"), tz = "UTC")
  lastTimeLastYr <- seq(firstTimeAfterLastYr, by = paste0("-", timeStep), length = 2)[2]
  times <- seq(firstTimeFirstYr, lastTimeLastYr, by = timeStep)

  return(times)
}
# #TEST
# # tester=extendDates(simLengthNyrs=100,dd=obs$day,mm=obs$month,yy=obs$year)
#
# makeDates<-function(datStart=NULL,
#                     datFinish=NULL){
#   print('need to update makeDates')
#   date_gen=seq(as.Date(datStart),as.Date(datFinish),by="day")
#   day <- as.numeric(format(date_gen,"%d"))
#   month<- as.numeric(format(date_gen,"%m"))
#   year<- as.numeric(format(date_gen,"%Y"))
#   dates=data.frame(year,month,day)
#   return(dates)
# }
#
# #get date info across multiple models
mod.get.date.ind <- function(obs = NULL,
                             modelTag = NULL,
                             modelInfo = NULL,
                             southHemi = TRUE) {
  nMod <- length(modelTag) # how many models

  datInd <- list()
  datInd[["obs"]] <- get.date.ind(times = obs$times, nperiod = 12, southHemi = southHemi) # make obs based datInd

  for (i in 1:nMod) {
    #    datInd[[modelTag[i]]]=get.date.ind(dd=obs$day,mm=obs$month,yy=obs$year,nperiod=modelInfo[[modelTag[i]]]$nperiod,southHemi=southHemi)    # FROM dateManager.R
    datInd[[modelTag[i]]] <- get.date.ind(times = obs$times, nperiod = modelInfo[[modelTag[i]]]$nperiod, southHemi = southHemi) # FROM dateManager.R
    # datInd[[modelTag[i]]]$i.mod=datInd[[modelTag[i]]]$i.pp  #add on i.mod
  }
  return(datInd)
}

# #get date info across multiple models - dates extended
# mod.get.date.ind.extnd<-function(obs=NULL,
#                                  dateExtnd=NULL,
#                                  modelTag=NULL,
#                                  modelInfo=NULL,
#                                  southHemi=TRUE,
#                                  simLengthNyrs=NULL,
#                                  file=NULL
#                                  ){
#   datInd=list()
#   datInd[["obs"]]=get.date.ind(dd=obs$day,mm=obs$month,yy=obs$year,nperiod=12,southHemi=southHemi)              #make obs based datInd
#   for(i in 1:length(modelTag)){
#     datInd[[modelTag[i]]]=get.date.ind(dd=dateExtnd$day,mm=dateExtnd$month,yy=dateExtnd$year,nperiod=modelInfo[[modelTag[i]]]$nperiod,southHemi=TRUE)          # FROM dateManager.R
#   }
#   return(datInd)
# }
#
# Get dat indices
get.date.ind <- function(times,
                         nperiod = NULL,
                         southHemi = TRUE) {
  if (is.null(nperiod)) {
    nperiod <- 1
  }

  nTimes <- length(times)
  yy <- as.integer(format(times, "%Y"))
  mm <- as.integer(format(times, "%m"))
  nyr <- yy[nTimes] - yy[1] + 1 # get number of years on record

  i.mm <- get.month.ind(mm = mm) # get indices for months
  i.yy <- get.year.ind(yy = yy, nyr = nyr) # get indices for years
  i.3yy <- get.nyear.ind(yy = yy, nyrEitherSide = 1) # get indices for 3 year moving window
  i.5yy <- get.nyear.ind(yy = yy, nyrEitherSide = 2) # get indices for 5 year moving window
  # if (nyr>=10){
  #   i.10yyBlock=get.nyearBlock.ind(yy=yy,inc=10)  #get indices for 10 year window
  # } else {
  #   i.10yyBlock = NULL
  # }
  # if (nyr>=50){
  #   i.50yyBlock=get.nyearBlock.ind(yy=yy,inc=50)  #get indices for 50 year window
  # } else {
  #   i.50yyBlock = NULL
  # }
  if (southHemi == TRUE) {
    i.ss <- get.seas.ind(i.mm = i.mm) # get indices for seasons
  } else {
    print("warning check seasons") # warning not southern hemisphere
  }
  # dateS=paste(yy[1],mm[1],dd[1],sep="-")
  # dateF=paste(yy[ndays],mm[ndays],dd[ndays],sep="-")
  # jj=julian_day(dateS=dateS,dateF=dateF)

  jj <- as.integer(format(times, "%j"))

  i.pp <- list()

  if ((nperiod == 1) | (nperiod == 4) | (nperiod == 12)) {
    if (nperiod == 1) {
      i.pp[[1]] <- seq(1, nTimes)
    } # annual model case
    if (nperiod == 4) {
      i.pp <- i.ss
    } # seasonal model case
    if (nperiod == 12) {
      i.pp <- i.mm
    } # monthly model case (not currently in use)
  } else {
    harInd <- split_ts(nperiod = nperiod, jj = jj) # alternative period split - calculate indices
    i.pp <- get.period.ind(har.period = harInd, nperiod = nperiod)
  }

  # datInd=list(ndays=ndays,
  #             nyr=nyr,
  #             i.mm=i.mm,
  #             i.yy=i.yy,
  #             i.3yy=i.3yy,
  #             i.5yy=i.5yy,
  #             i.10yyBlock=i.10yyBlock,
  #             i.50yyBlock=i.50yyBlock,
  #             i.ss=i.ss,
  #             i.pp=i.pp,
  #             jj=jj)

  datInd <- list(
    nTimes = nTimes,
    nyr = nyr,
    i.mm = i.mm,
    i.yy = i.yy,
    i.ss = i.ss,
    i.pp = i.pp,
    i.3yy = i.3yy,
    i.5yy = i.5yy,
    jj = jj
  )

  return(datInd)
}

get.month.ind <- function(mm = NULL # ts vector of months
) {
  i.mm <- NULL
  for (m in 1:12) i.mm[[m]] <- which(mm == m) # CREATE MONTHLY INDICES
  return(i.mm)
}

get.year.ind <- function(yy = NULL, # ts vector of years
                         nyr = NULL # nyears oon record
) {
  years <- seq(yy[1], yy[length(yy)]) # GET VECTOR OF YEARS
  i.yy <- NULL
  for (Y in 1:nyr) i.yy[[Y]] <- which(yy == years[Y]) # CREATE MONTHLY INDICES
  return(i.yy)
}

get.nyear.ind <- function(yy = NULL, # ts vector of years
                          nyrEitherSide = NULL) {
  years <- seq(min(yy) + nyrEitherSide, max(yy) - nyrEitherSide)
  nyr <- length(years)
  i.yywin <- NULL
  for (Y in 1:nyr) i.yywin[[Y]] <- which((yy >= years[Y] - nyrEitherSide) & (yy <= years[Y] + nyrEitherSide)) # CREATE MONTHLY INDICES
  return(i.yywin)
}

# get.nyearBlock.ind<-function(yy=NULL,   # ts vector of years
#                        inc=1
# ){
#   start=seq(yy[1],max(yy),by=inc)
#   end=seq(yy[1]+inc-1,max(yy),by=inc)
#   nBlock = length(end)
#   start=start[1:nBlock] # make sure same number of starts and ends
#   i.yy=NULL
#   for(Y in 1:nBlock) i.yy[[Y]]=which((yy>=start[Y])&(yy<=end[Y]))            # CREATE MONTHLY INDICES
#   return(i.yy)
# }
#
get.seas.ind <- function(i.mm = NULL # list of days sorted by month
) {
  # NOTE SOUTHERN HEMISPHERE HERE
  # define months belonging to each season
  seas <- t(matrix(
    c(
      9, 10, 11, # SPR -SON
      12, 1, 2, # SUM -DJF
      3, 4, 5, # AUT -MAM
      6, 7, 8
    ), # WIN -JJA
    nrow = 3, ncol = 4
  ))

  i.ss <- NULL
  for (s in 1:4) i.ss[[s]] <- c(i.mm[[seas[s, 1]]], i.mm[[seas[s, 2]]], i.mm[[seas[s, 3]]]) # CREATE SEASONAL INDICES i.ss[[1]]-i.ss[[4]] (not contiguous, needs a sort)
  for (s in 1:4) {
    tmp <- i.ss[[s]]
    tmp <- sort(tmp)
    i.ss[[s]] <- tmp # put in daily order
  }
  rm(tmp)
  return(i.ss)
}

# julian_day<-function(dateS=NULL,  #start date e.g. "1995-01-01"
#                      dateF=NULL
# ){
#   date_gen <- seq(as.Date(dateS),as.Date(dateF),by="day")
#   ndays <- length(date_gen)
#   jj <- as.numeric(format(date_gen,"%j"))
#   return(jj)
# }
# # jj=julian_day(dateS="1995-01-01",dateF="2004-12-31")

split_ts <- function(nperiod = 26, # no. of periods to divide year over
                     jj = NULL # vector of julian day values
) {
  nd.per <- floor(366 / nperiod) # no. days in period
  nd.year <- nperiod * nd.per # est no. days in year
  short <- 366 - nd.year # no. days short from 366

  indl <- NULL
  for (i in 1:nperiod) {
    indl <- c(indl, rep(i, nd.per))
  }
  indl <- c(indl, rep(nperiod, short)) # add missing days on end

  harInd <- rep(NA, length(jj))
  for (i in 1:366) {
    tmpInd <- which(jj == i)
    harInd[tmpInd] <- indl[i] # determine which day belongs to which period
  }
  return(harInd)
}
# harInd= split_ts(nperiod=26,jj=jj)

get.period.ind <- function(har.period = NULL, # ts vector of period assigned
                           nperiod = NULL # number of periods used
) {
  i.hh <- NULL
  for (h in 1:nperiod) i.hh[[h]] <- which(har.period == h) # CREATE MONTHLY INDICES
  return(i.hh)
}
# i.hh=get.period.ind(har.period=harInd,nperiod=26)
