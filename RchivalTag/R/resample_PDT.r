
resample_DepthTempTS <- function(ts_df, ...){
  m <- interpolate_TempDepthProfiles(ts_df, verbose=FALSE, ...)
  out <- .resample_interpDepthTempTS(ts_df, m)
  return(out)
}

resample_PDT <- function(ts_df, PDT, ...){
  
  m <- interpolate_PDTs(PDT, verbose=FALSE, ...)  
  out <- .resample_interpDepthTempTS(ts_df, m)
  return(out)
  }
  
.resample_interpDepthTempTS <- function(ts_df,m){
  ts_df2 <- ts_df
  ts_df2$Temperature <- NA
  M <- m$station.1$Temperature_matrix
  n <- which(!is.na(ts_df$Depth))[1]
  for(n in which(!is.na(ts_df$Depth))){
    i <- which(m$station.1$Date == ts_df$date[n])
    j <- which(m$station.1$Depth == ts_df$Depth[n])
    ts_df2$Temperature[n] <- round(M[j,i],1)
  }
  return(ts_df2)
}