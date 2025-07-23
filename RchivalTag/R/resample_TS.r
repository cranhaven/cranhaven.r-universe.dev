
resample_TS <- function(df, tstep, nsims){
  tsims <- list()
  tstep0 <- tstep
  min_tstep <- unique(as.numeric(diff(df$datetime[1:10]))) #"corresponds to raw data sets!"
  if(tstep %% min_tstep != 0) stop('selected time step (',tstep,'s)is not a multiple of the original sampling resolution (',min_tstep,'s). Please revise!')
  if(tstep == 0) tstep0 <- min_tstep
  
  df$datetime <- as.POSIXct(df$datetime,tz = 'UTC')
  df$date <- as.Date(df$datetime)
  df$year <- as.numeric(format(df$date, "%Y"))
  df$month <- as.numeric(format(df$date, "%m"))
  df$day <- as.numeric(format(df$date, "%d"))
  df$tstep <- tstep0

  tstarts <- which(df$datetime < df$datetime[1]+tstep0)
  tag <- paste(df[1,which(names(df) %in% c('Serial','tag',"Ptt","DeployID"))],collapse=" - ")
  # head(df)
  tstart <- tstarts[1]
  if(missing(nsims)) nsims <- length(tstarts)
  if(nsims > length(tstarts)) nsims <- length(tstarts)
  if(nsims < length(tstarts)) tstarts <- tstarts[1:nsims]
  for(tstart in tstarts){
    cat('resampling time series data from tag',tag,'with time step', tstep, 's - repetition',tstart,"of", tail(tstarts,1),'\n')
    ii <- which(as.character(df$datetime) %in% as.character(seq(df$datetime[tstart],df$datetime[nrow(df)],by=tstep0)))
    ts <- df[ii,]
    tsims[[tstart]] <- ts
  }
  return(tsims)
}