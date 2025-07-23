get_DayTimeLimits <- function(pos){
  if(any(!(c("Lon","Lat","datetime") %in% names(pos)))) stop("no geolocation data or datetime vector provided (Lon, Lat, datetime)! please revise.")
  
  pos$datetime <- as.POSIXct(pos$datetime,tz = "UTC")
  pos$sunrise <- sunriset(cbind(pos$Lon,pos$Lat), pos$datetime, direction="sunrise", POSIXct.out=TRUE)$time
  pos$sunset <- sunriset(cbind(pos$Lon,pos$Lat), pos$datetime, direction="sunset", POSIXct.out=TRUE)$time
  pos$dawn.naut <- crepuscule(cbind(pos$Lon,pos$Lat), pos$datetime, solarDep=12, direction="dawn", POSIXct.out=TRUE)$time
  pos$dawn.ast <- crepuscule(cbind(pos$Lon,pos$Lat), pos$datetime, solarDep=18, direction="dawn", POSIXct.out=TRUE)$time
  
  head(pos)
  ii <- which(as.Date(pos$sunrise) < as.Date(pos$datetime))
  if(length(ii) > 0) {
    pos$sunrise[ii] <- sunriset(cbind(pos$Lon,pos$Lat), pos$datetime+24*60*60, direction="sunrise", POSIXct.out=TRUE)$time
    pos$dawn.naut <- crepuscule(cbind(pos$Lon,pos$Lat), pos$datetime+24*60*60, solarDep=12, direction="dawn", POSIXct.out=TRUE)$time
    pos$dawn.ast <- crepuscule(cbind(pos$Lon,pos$Lat), pos$datetime+24*60*60, solarDep=18, direction="dawn", POSIXct.out=TRUE)$time
  }
  pos$dusk.naut <- crepuscule(cbind(pos$Lon,pos$Lat), pos$datetime, solarDep=12, direction="dusk", POSIXct.out=TRUE)$time
  pos$dusk.ast <- crepuscule(cbind(pos$Lon,pos$Lat), pos$datetime, solarDep=18, direction="dusk", POSIXct.out=TRUE)$time
  
  return(pos)
}


classify_DayTime <- function(pos, twilight.set="ast"){
  dawn <- paste0('dawn.',twilight.set)
  dusk <- paste0('dusk.',twilight.set)
  
  if(!all(c(dawn, dusk, 'sunrise','sunset') %in% names(pos))) pos <- get_DayTimeLimits(pos)
  pos$daytime <- 'Day'
  pos$daytime[which(pos$sunrise > pos$sunset & (pos$datetime < pos$sunrise & pos$datetime >= pos$sunset))] <- 'Night'
  pos$daytime[which(pos$sunset > pos$sunrise & (pos$datetime < pos$sunrise | pos$datetime >= pos$sunset))] <- 'Night'
  pos$daytime.long <- pos$daytime
  pos$daytime.long[which(pos$datetime >= pos[[dawn]] & pos$datetime < pos$sunrise)] <- 'Dawn'
  pos$daytime.long[which(pos$datetime >= pos$sunset & pos$datetime < pos[[dusk]])] <- 'Dusk'
  # pos$daytime.long[which(pos$datetime >= pos[[dusk]] & pos$datetime < pos$sunset)] <- 'Dusk'
  
  
  return(pos)
}