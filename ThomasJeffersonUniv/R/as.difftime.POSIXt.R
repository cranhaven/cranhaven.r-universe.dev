

as.difftime.POSIXct <- function(tim) {
  as.difftime.POSIXlt(as.POSIXlt.POSIXct(tim))
}

as.difftime.POSIXlt <- function(tim) {
  # see ?base::ISOdatetime
  #if (!all(tim$year == -1L, na.rm = TRUE) ||
  #    !all(tim$mon == 11L, na.rm = TRUE) ||
  #    !all(tim$mday == 31L, na.rm = TRUE)) stop('should not use [as.difftime.POSIXlt]')
  
  tim2 <- tim
  zero_int <- rep(0L, times = length(tim))
  zero_int[is.na(tim)] <- NA_integer_
  zero_dbl <- rep(0, times = length(tim))
  zero_dbl[is.na(tim)] <- NA_real_
  tim2$hour <- tim2$min <- zero_int
  tim2$sec <- zero_dbl
     
  difftime(time1 = tim, time2 = tim2, units = 'auto')
}