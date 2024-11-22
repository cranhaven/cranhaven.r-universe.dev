##
##    Metric to calculate maximum sample range for a Stream of seismic data
##

maxRangeMetric <- function(st, window=300, increment=150) {

   if (increment >= window) {
      warning("For best results, increment value should be less than the window value.")
   }
   snclq <- st@traces[[1]]@id
   starttime <- st@requestedStarttime
   endtime <- st@requestedEndtime
   x <- mergeTraces(st)@traces[[1]]

   n_samp <- round(window * x@stats@sampling_rate)  # must have integer number of samples
   n_incr <- round(increment * x@stats@sampling_rate)

   # last rolling segment needs to be length of window or it will be skipped, add NAs as needed
   modinc <- length(x@data) %% n_incr
   if (modinc == 0) {
      x@data <- c(x@data,rep(NA,n_samp-n_incr))
   } else {
      x@data <- c(x@data,rep(NA,n_samp-modinc))
   }

   range <- seismicRoll::roll_range(x@data,n_samp,n_incr, align="left")
   maxRange <- max(range,na.rm=T)

   m1 <- new("GeneralValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="max_range", elementNames=c("value"), elementValues=maxRange)
   return(c(m1))
}
