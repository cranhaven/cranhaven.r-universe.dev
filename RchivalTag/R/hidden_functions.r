.switch_if <- function(x,a,b){
  if(x){
    y <- a
  }else{
    y <- b
  }
  return(y)
}

.fact2num <- function(x){  
  as.numeric(as.character(x))
}

.fact2Date <- function(x, date_format="%Y-%m-%d", lang_format="en") {
  x0 <- x
  LOCALE <- readr::locale(lang_format)
  LOCALE$date_names$mon_ab <- gsub("\\.","",LOCALE$date_names$mon_ab)
  LOCALE$date_names$day_ab <- gsub("\\.","",LOCALE$date_names$day_ab)
  out <- readr::parse_date(x, format = date_format, locale = LOCALE)
  
  out <- readr::parse_date(x, date_format, locale = LOCALE)
  i <- is.na(out)
  if(any(i)) {
    # print(head(x0[i]))
    stop(paste("date concersion failed! Please revise current'date_format':",date_format,"to",x0[which(i)[1]]))
  }
  return(out)
}


.fact2datetime <- function(x, date_format="%Y-%m-%d %H:%M:%S", tz="UTC", lang_format="en") {
  x0 <- x <- as.character(x)
  LOCALE <- readr::locale(lang_format)
  LOCALE$date_names$mon_ab <- gsub("\\.","",LOCALE$date_names$mon_ab)
  LOCALE$date_names$day_ab <- gsub("\\.","",LOCALE$date_names$day_ab)
  out <- suppressWarnings(readr::parse_datetime(x, format = date_format, locale = LOCALE))
  i <- is.na(out)
  j <- which(i)
  
  if(any(i)) {
    out[j] <- readr::parse_datetime(paste(x[j],"00:00:00"), format = date_format, locale = LOCALE)
  }
  if(tz != "UTC"){
    x <- out
    out <- as.POSIXct(strptime(as.character(x),"%Y-%m-%d %H:%M:%S",tz=tz),tz = tz)
    i <- which(is.na(out))
    out[i] <- as.POSIXct(strptime(paste(x[i],"00:00:00"),"%Y-%m-%d %H:%M:%S",tz=tz),tz = tz)
  }
  # if(tz != "UTC"){
  #   out <- lubridate::with_tz(out,tzone=tz)
  # }
  i <- is.na(out)
  if(any(i)) {
    # print(head(x0[i]))
    stop(paste("date concersion failed! Please revise current'date_format':",date_format,"to",x0[which(i)[1]]))
  }
  
  # if(length(out) == 0) {
  #   # print(head(x0[i]))
  #   stop(paste("date concersion failed! Please revise current'date_format':",date_format,"to",x0[1]))
  # }
  
  return(out)
}

.datetime2min.dc <- function(x,tz = "UTC"){
  as.numeric(format(x,"%M",tz = tz))+as.numeric(format(x,"%S",tz = tz))/60
}

.datetime2hour <- function(x,tz="UTC"){
  as.numeric(format(x,"%H",tz = tz))
}

.datetime2hour.dc <- function(x,tz="UTC"){
  as.numeric(format(x,"%H",tz = tz))+as.numeric(format(x,"%M",tz = tz))/60+as.numeric(format(x,"%S",tz = tz))/(60*60)
}

.datetime2min <- function(x,tz="UTC"){
  as.numeric(format(x,"%M",tz = tz))
}

.num2datetime <- function(x,tz="UTC",hours.offset=0){
  out <- .fact2datetime("1970-01-01 00:00:00",tz=tz)+x+hours.offset*(60*60)
  return(out)
}

.num2date <- function(x,tz = "UTC"){
  as.Date(x,origin="1970-01-01",tz = tz)
}


.num2month <- function(m,english=T,abbrev=F){
  if(english) lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  dat <- m
  if(!is(m,"Date")) dat <- as.Date(paste0("2012-",m,"-1"))
  Dat <- format(as.Date(dat),"%B")
  if(abbrev) Dat <- format(as.Date(dat),"%b")
  return(Dat)
}

.date2year <- function(x,tz = "UTC") as.numeric(format(x,"%Y",tz = tz))

.date2datetime <- function(x,tz="",midday=T){
  
  sstart <- 12
  if(!midday) sstart <- 0
  strptime(paste(.fact2Date(as.character(x)),paste0(sstart,":00:00")),"%Y-%m-%d %H:%M:%S",tz = tz)
}

.diff.time <- function(x,units="secs"){
  n <- length(x)
  out <- rep(NA,n)
  for(i in 2:n){
    out[i] <- difftime(x[i],x[i-1],units = units)
  }
  return(out)
}

.makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

# .get_histos_stats <- function(df, bin_breaks){
#   
#   nbins <- length(bin_breaks)-1
#   vbins <- paste0("Bin",1:nbins)
#   mids <- bin_breaks[1:nbins]+diff(bin_breaks)/2
# 
#   df$SD <- df$avg <- NA
#   for(i in 1:nrow(df)){
#     s <- c()
#     for(j in 2:length(vbins)){
#       t <- df[[vbins[j]]][i]*86 # theoreticaly 8640 depth records per day if sampled every 10s
#       s <- c(s,rep(mids[j],t))
#     }
#     df$avg[i] <- mean(s,na.rm=T)
#     df$SD[i] <- sd(s,na.rm=T)
#   }
#   df.new <- df
#   return(df.new)
# }


