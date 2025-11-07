#### Angles ####
#degrees to radians
d2r <- function(x){x*pi/180}

#radians to degrees
r2d <- function(x){x*180/pi}

#hours to radians
h2r <- function(x){x*pi/12}

#hours to degrees
h2d <- function(x){x*180/12}

#radians to hours
r2h <- function(x){x*12/pi}

#degrees to hours
d2h <- function(x){x*12/180}

#radians to seconds
r2sec <- function(x){x*12/pi*3600}

#radians to minutes
r2min <- function(x){x*12/pi*60}

#### Time ###

#hours minutes and seconds to hours
hms <- function(x)
{
    hour(x)+minute(x)/60+second(x)/3600
}

#day of the year
doy <- function(x){
  as.numeric(format(x, '%j'))
}

#day of the month
dom <- function(x){
  as.numeric(format(x, '%d'))
}

#trunc days
truncDay <- function(x){as.POSIXct(trunc(x, units='days'))}

## Check if daily indexes are equal (used in fCompD and fTemp)
checkIndexD <- function(ix, iy)
{
    dx <- truncDay(ix)
    dy <- truncDay(iy)
    test <- all.equal(dx, dy,  check.attributes = FALSE)
    if (!isTRUE(test))
        stop('daily indexes do not match.')
}

##difftime to hours
diff2Hours  <- function(by){
  if (!inherits(by, 'difftime')) {
    stop('This function is only useful for difftime objects.')
  } else {
    return(as.numeric(by, units='hours'))
  }
}

# character to difftime
char2diff <- function(by){
  if (!is.character(by)) {
    stop('This function is only useful for character strings.')
  } else {
    ##Adapted from seq.POSIXt
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L) 
      stop("invalid 'by' string")
    units <- c("secs", "mins", "hours")
    valid <- pmatch(by2[length(by2)], units)
    if (is.na(valid)) {
      stop("invalid string for 'by'")
    } else {
      unitValid <- units[valid]
      if (length(by2)==1) {
        by2=1
      } else {
        by2=as.numeric(by2[1])
      }
      result <- as.difftime(by2,units=unitValid)
      return(result)
    }
  }
}

# sample to hours
sample2Hours <- function(by){
    if (is.character(by)) {
        y <- char2diff(by)
        return(diff2Hours(y))
    } else if (inherits(by, 'difftime')) {
        return(diff2Hours(by))
    } else {stop('by must be a character or difftime.')}
}

# Day Of Month
DOM <- function(x){
    if('Dates' %in% names(x)){
        d1 <- as.Date(x$Dates)
    }else{
        d1 <- as.Date(paste(x$year, x$month, 1, sep = '-'))
    }
    d2 <- as.Date(sapply(d1, function(x) seq(x, by = '1 month', length = 2)[2]))
    DOM <- as.numeric(d2-d1)
    return(DOM)
}

#### Energy ####
# Power to energy
P2E <- function(x, by){
    Nm <- 1/sample2Hours(by)
    sum(x, na.rm = 1)/Nm
}

solvePac <- function(x, Cinv){
    Vdc <- x[1]
    PdcN <- x[2]
    V <- c(1, Vdc, Vdc^2)
    Ki <- t(colSums(V*t(Cinv)))
    A <- Ki[3]
    B <- Ki[2]+1
    C <- Ki[1]-(PdcN)
    result <- (-B+sqrt(B^2-4*A*C))/(2*A)
    return(result)
}

dst <- function(x) #Daylight Savings Time
   {
     as.POSIXlt(x)$isdst
   }

lonHH<-function(tz)
    { #Calculates the longitude (in radians) of a time zone.
      stopifnot(class(tz)=='character')
      tHH <- as.POSIXct('2000-1-1 12:00:00', tz=tz)
      tUTC <- as.POSIXct(format(tHH, tz='UTC'), tz=tz)
      h2r(as.numeric(tHH-tUTC))
    }

local2Solar <- function(x, lon=NULL){	
  tz=attr(x, 'tzone')
  if (tz == '' || is.null(tz)) {tz='UTC'}
  ##Daylight savings time
  AO <- 3600*dst(x)
  AOneg <- (AO<0)
  if (any(AOneg)) {
    AO[AOneg] <- 0
    warning('Some Daylight Savings Time unknown. Set to zero.')
  }
  ##Difference between local longitude and time zone longitude LH
  LH <- lonHH(tz)
  if (is.null(lon)) 
    {deltaL <- 0
   } else
  {deltaL <- d2r(lon)-LH
 }
  ##Local time corrected to UTC
  tt <- format(x, tz=tz)
  result <- as.POSIXct(tt, tz='UTC')-AO+r2sec(deltaL)
  result
}
